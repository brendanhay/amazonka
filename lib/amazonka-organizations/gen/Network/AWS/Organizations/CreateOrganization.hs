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
-- Module      : Network.AWS.Organizations.CreateOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS organization. The account whose user is calling the CreateOrganization operation automatically becomes the <http://docs.aws.amazon.com/IAM/latest/UserGuide/orgs_getting-started_concepts.html#account master account> of the new organization.
--
--
-- This operation must be called using credentials from the account that is to become the new organization's master account. The principal must also have the relevant IAM permissions.
--
-- By default (or if you set the @FeatureSet@ parameter to @ALL@ ), the new organization is created with all features enabled and service control policies automatically enabled in the root. If you instead choose to create the organization supporting only the consolidated billing features by setting the @FeatureSet@ parameter to @CONSOLIDATED_BILLING"@ , then no policy types are enabled by default and you cannot use organization policies.
--
module Network.AWS.Organizations.CreateOrganization
    (
    -- * Creating a Request
      createOrganization
    , CreateOrganization
    -- * Request Lenses
    , coFeatureSet

    -- * Destructuring the Response
    , createOrganizationResponse
    , CreateOrganizationResponse
    -- * Response Lenses
    , corsOrganization
    , corsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createOrganization' smart constructor.
newtype CreateOrganization = CreateOrganization'
  { _coFeatureSet :: Maybe OrganizationFeatureSet
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coFeatureSet' - Specifies the feature set supported by the new organization. Each feature set supports different levels of functionality.     * /CONSOLIDATED_BILLING/ : All member accounts have their bills consolidated to and paid by the master account. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated Billing> in the /AWS Organizations User Guide/ .     * /ALL/ : In addition to all the features supported by the consolidated billing feature set, the master account can also apply any type of policy to any member account in the organization. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features> in the /AWS Organizations User Guide/ .
createOrganization
    :: CreateOrganization
createOrganization = CreateOrganization' {_coFeatureSet = Nothing}


-- | Specifies the feature set supported by the new organization. Each feature set supports different levels of functionality.     * /CONSOLIDATED_BILLING/ : All member accounts have their bills consolidated to and paid by the master account. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated Billing> in the /AWS Organizations User Guide/ .     * /ALL/ : In addition to all the features supported by the consolidated billing feature set, the master account can also apply any type of policy to any member account in the organization. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features> in the /AWS Organizations User Guide/ .
coFeatureSet :: Lens' CreateOrganization (Maybe OrganizationFeatureSet)
coFeatureSet = lens _coFeatureSet (\ s a -> s{_coFeatureSet = a})

instance AWSRequest CreateOrganization where
        type Rs CreateOrganization =
             CreateOrganizationResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 CreateOrganizationResponse' <$>
                   (x .?> "Organization") <*> (pure (fromEnum s)))

instance Hashable CreateOrganization where

instance NFData CreateOrganization where

instance ToHeaders CreateOrganization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.CreateOrganization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateOrganization where
        toJSON CreateOrganization'{..}
          = object
              (catMaybes [("FeatureSet" .=) <$> _coFeatureSet])

instance ToPath CreateOrganization where
        toPath = const "/"

instance ToQuery CreateOrganization where
        toQuery = const mempty

-- | /See:/ 'createOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { _corsOrganization   :: !(Maybe Organization)
  , _corsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corsOrganization' - A structure that contains details about the newly created organization.
--
-- * 'corsResponseStatus' - -- | The response status code.
createOrganizationResponse
    :: Int -- ^ 'corsResponseStatus'
    -> CreateOrganizationResponse
createOrganizationResponse pResponseStatus_ =
  CreateOrganizationResponse'
    {_corsOrganization = Nothing, _corsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the newly created organization.
corsOrganization :: Lens' CreateOrganizationResponse (Maybe Organization)
corsOrganization = lens _corsOrganization (\ s a -> s{_corsOrganization = a})

-- | -- | The response status code.
corsResponseStatus :: Lens' CreateOrganizationResponse Int
corsResponseStatus = lens _corsResponseStatus (\ s a -> s{_corsResponseStatus = a})

instance NFData CreateOrganizationResponse where
