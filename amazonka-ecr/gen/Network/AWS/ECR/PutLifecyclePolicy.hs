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
-- Module      : Network.AWS.ECR.PutLifecyclePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a lifecycle policy. For information about lifecycle policy syntax, see <http://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html Lifecycle Policy Template> .
--
--
module Network.AWS.ECR.PutLifecyclePolicy
    (
    -- * Creating a Request
      putLifecyclePolicy
    , PutLifecyclePolicy
    -- * Request Lenses
    , plpRegistryId
    , plpRepositoryName
    , plpLifecyclePolicyText

    -- * Destructuring the Response
    , putLifecyclePolicyResponse
    , PutLifecyclePolicyResponse
    -- * Response Lenses
    , plprsRegistryId
    , plprsLifecyclePolicyText
    , plprsRepositoryName
    , plprsResponseStatus
    ) where

import Network.AWS.ECR.Types
import Network.AWS.ECR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { _plpRegistryId          :: !(Maybe Text)
  , _plpRepositoryName      :: !Text
  , _plpLifecyclePolicyText :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plpRegistryId' - The AWS account ID associated with the registry that contains the repository. If you do
