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
-- Module      : Network.AWS.ECR.GetLifecyclePolicyPreview
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of the specified lifecycle policy preview request.
--
--
module Network.AWS.ECR.GetLifecyclePolicyPreview
    (
    -- * Creating a Request
      getLifecyclePolicyPreview
    , GetLifecyclePolicyPreview
    -- * Request Lenses
    , glppRegistryId
    , glppImageIds
    , glppNextToken
    , glppFilter
    , glppMaxResults
    , glppRepositoryName

    -- * Destructuring the Response
    , getLifecyclePolicyPreviewResponse
    , GetLifecyclePolicyPreviewResponse
    -- * Response Lenses
    , glpprsSummary
    , glpprsStatus
    , glpprsRegistryId
    , glpprsLifecyclePolicyText
    , glpprsNextToken
    , glpprsRepositoryName
    , glpprsPreviewResults
    , glpprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLifecyclePolicyPreview' smart constructor.
data GetLifecyclePolicyPreview = GetLifecyclePolicyPreview'
  { _glppRegistryId     :: !(Maybe Text)
  , _glppImageIds       :: !(Maybe [ImageIdentifier])
  , _glppNextToken      :: !(Maybe Text)
  , _glppFilter         :: !(Maybe LifecyclePolicyPreviewFilter)
  , _glppMaxResults     :: !(Maybe Nat)
  , _glppRepositoryName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLifecyclePolicyPreview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glppRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do not specify a registry, the default registry is assumed.
--
-- * 'glppImageIds' - The list of imageIDs to be included.
--
-- * 'glppNextToken' - The @nextToken@ value returned from a previous paginated
