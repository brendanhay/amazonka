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
-- Module      : Network.AWS.Organizations.EnableAllFeatures
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables all features in an organization. This enables the use of organization policies that can restrict the services and actions that can be called in each account. Until you enable all features, you have access only to consolidated billing, and you can't use any of the advanced account administration features that AWS Organizations supports. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
--
--
-- /Important:/ This operation is required only for organizations that were created explicitly with only the consolidated billing features enabled, or that were migrated from a Consolidated Billing account family to Organizations. Calling this operation sends a handshake to every invited account in the organization. The feature set change can be finalized and the additional features enabled only after all administrators in the invited accounts approve the change by accepting the handshake.
--
-- After you enable all features, you can separately enable or disable individual policy types in a root using 'EnablePolicyType' and 'DisablePolicyType' . To see the status of policy types in a root, use 'ListRoots' .
--
-- After all invited member accounts accept the handshake, you finalize the feature set change by accepting the handshake that contains @"Action": "ENABLE_ALL_FEATURES"@ . This completes the change.
--
-- After you enable all features in your organization, the master account in the organization can apply policies on all member accounts. These policies can restrict what users and even administrators in those accounts can do. The master account can apply policies that prevent accounts from leaving the organization. Ensure that your account administrators are aware of this.
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.EnableAllFeatures
    (
    -- * Creating a Request
      enableAllFeatures
    , EnableAllFeatures

    -- * Destructuring the Response
    , enableAllFeaturesResponse
    , EnableAllFeaturesResponse
    -- * Response Lenses
    , eafrsHandshake
    , eafrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableAllFeatures' smart constructor.
data EnableAllFeatures =
  EnableAllFeatures'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAllFeatures' with the minimum fields required to make a request.
--
enableAllFeatures
    :: EnableAllFeatures
enableAllFeatures = EnableAllFeatures'


instance AWSRequest EnableAllFeatures where
        type Rs EnableAllFeatures = EnableAllFeaturesResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 EnableAllFeaturesResponse' <$>
                   (x .?> "Handshake") <*> (pure (fromEnum s)))

instance Hashable EnableAllFeatures where

instance NFData EnableAllFeatures where

instance ToHeaders EnableAllFeatures where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.EnableAllFeatures" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableAllFeatures where
        toJSON = const (Object mempty)

instance ToPath EnableAllFeatures where
        toPath = const "/"

instance ToQuery EnableAllFeatures where
        toQuery = const mempty

-- | /See:/ 'enableAllFeaturesResponse' smart constructor.
data EnableAllFeaturesResponse = EnableAllFeaturesResponse'
  { _eafrsHandshake      :: !(Maybe Handshake)
  , _eafrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAllFeaturesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eafrsHandshake' - A structure that contains details about the handshake created to support this request to enable all features in the organization.
--
-- * 'eafrsResponseStatus' - -- | The response status code.
enableAllFeaturesResponse
    :: Int -- ^ 'eafrsResponseStatus'
    -> EnableAllFeaturesResponse
enableAllFeaturesResponse pResponseStatus_ =
  EnableAllFeaturesResponse'
    {_eafrsHandshake = Nothing, _eafrsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the handshake created to support this request to enable all features in the organization.
eafrsHandshake :: Lens' EnableAllFeaturesResponse (Maybe Handshake)
eafrsHandshake = lens _eafrsHandshake (\ s a -> s{_eafrsHandshake = a})

-- | -- | The response status code.
eafrsResponseStatus :: Lens' EnableAllFeaturesResponse Int
eafrsResponseStatus = lens _eafrsResponseStatus (\ s a -> s{_eafrsResponseStatus = a})

instance NFData EnableAllFeaturesResponse where
