{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all IAM users, groups, and roles that the specified managed policy is attached to.
--
--
-- You can use the optional @EntityFilter@ parameter to limit the results to a particular type of entity (users, groups, or roles). For example, to list only the roles that are attached to the specified policy, set @EntityFilter@ to @Role@ .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListEntitiesForPolicy
    (
    -- * Creating a Request
      listEntitiesForPolicy
    , ListEntitiesForPolicy
    -- * Request Lenses
    , lefpPathPrefix
    , lefpEntityFilter
    , lefpMarker
    , lefpMaxItems
    , lefpPolicyUsageFilter
    , lefpPolicyARN

    -- * Destructuring the Response
    , listEntitiesForPolicyResponse
    , ListEntitiesForPolicyResponse
    -- * Response Lenses
    , lefprsPolicyGroups
    , lefprsPolicyRoles
    , lefprsMarker
    , lefprsPolicyUsers
    , lefprsIsTruncated
    , lefprsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
  { _lefpPathPrefix        :: !(Maybe Text)
  , _lefpEntityFilter      :: !(Maybe EntityType)
  , _lefpMarker            :: !(Maybe Text)
  , _lefpMaxItems          :: !(Maybe Nat)
  , _lefpPolicyUsageFilter :: !(Maybe PolicyUsageType)
  , _lefpPolicyARN         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEntitiesForPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lefpPathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'lefpEntityFilter' - The entity type to use for filtering the results. For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
--
-- * 'lefpMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lefpMaxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- * 'lefpPolicyUsageFilter' - The policy usage method to use for filtering the results. To list only permissions policies, set
