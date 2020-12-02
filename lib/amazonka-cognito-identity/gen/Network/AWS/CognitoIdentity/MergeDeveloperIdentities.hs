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
-- Module      : Network.AWS.CognitoIdentity.MergeDeveloperIdentities
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two users having different @IdentityId@ s, existing in the same identity pool, and identified by the same developer provider. You can use this action to request that discrete users be merged and identified as a single user in the Cognito environment. Cognito associates the given source user (@SourceUserIdentifier@ ) with the @IdentityId@ of the @DestinationUserIdentifier@ . Only developer-authenticated users can be merged. If the users to be merged are associated with the same public provider, but as two different users, an exception will be thrown.
--
--
-- You must use AWS Developer credentials to call this API.
--
module Network.AWS.CognitoIdentity.MergeDeveloperIdentities
    (
    -- * Creating a Request
      mergeDeveloperIdentities
    , MergeDeveloperIdentities
    -- * Request Lenses
    , mdiSourceUserIdentifier
    , mdiDestinationUserIdentifier
    , mdiDeveloperProviderName
    , mdiIdentityPoolId

    -- * Destructuring the Response
    , mergeDeveloperIdentitiesResponse
    , MergeDeveloperIdentitiesResponse
    -- * Response Lenses
    , mdirsIdentityId
    , mdirsResponseStatus
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.CognitoIdentity.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to the @MergeDeveloperIdentities@ action.
--
--
--
-- /See:/ 'mergeDeveloperIdentities' smart constructor.
data MergeDeveloperIdentities = MergeDeveloperIdentities'
  { _mdiSourceUserIdentifier      :: !Text
  , _mdiDestinationUserIdentifier :: !Text
  , _mdiDeveloperProviderName     :: !Text
  , _mdiIdentityPoolId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergeDeveloperIdentities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdiSourceUserIdentifier' - User identifier for the source user. The value should be a @DeveloperUserIdentifier@ .
--
-- * 'mdiDestinationUserIdentifier' - User identifier for the destination user. The value should be a @DeveloperUserIdentifier@ .
--
-- * 'mdiDeveloperProviderName' - The "domain" by which Cognito will refer to your users. This is a (pseudo) domain name that you provide while creating an identity pool. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (.), underscore (_), and dash (-).
--
-- * 'mdiIdentityPoolId' - An identity pool ID in the format REGION:GUID.
mergeDeveloperIdentities
    :: Text -- ^ 'mdiSourceUserIdentifier'
    -> Text -- ^ 'mdiDestinationUserIdentifier'
    -> Text -- ^ 'mdiDeveloperProviderName'
    -> Text -- ^ 'mdiIdentityPoolId'
    -> MergeDeveloperIdentities
mergeDeveloperIdentities pSourceUserIdentifier_ pDestinationUserIdentifier_ pDeveloperProviderName_ pIdentityPoolId_ =
  MergeDeveloperIdentities'
    { _mdiSourceUserIdentifier = pSourceUserIdentifier_
    , _mdiDestinationUserIdentifier = pDestinationUserIdentifier_
    , _mdiDeveloperProviderName = pDeveloperProviderName_
    , _mdiIdentityPoolId = pIdentityPoolId_
    }


-- | User identifier for the source user. The value should be a @DeveloperUserIdentifier@ .
mdiSourceUserIdentifier :: Lens' MergeDeveloperIdentities Text
mdiSourceUserIdentifier = lens _mdiSourceUserIdentifier (\ s a -> s{_mdiSourceUserIdentifier = a})

-- | User identifier for the destination user. The value should be a @DeveloperUserIdentifier@ .
mdiDestinationUserIdentifier :: Lens' MergeDeveloperIdentities Text
mdiDestinationUserIdentifier = lens _mdiDestinationUserIdentifier (\ s a -> s{_mdiDestinationUserIdentifier = a})

-- | The "domain" by which Cognito will refer to your users. This is a (pseudo) domain name that you provide while creating an identity pool. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (.), underscore (_), and dash (-).
mdiDeveloperProviderName :: Lens' MergeDeveloperIdentities Text
mdiDeveloperProviderName = lens _mdiDeveloperProviderName (\ s a -> s{_mdiDeveloperProviderName = a})

-- | An identity pool ID in the format REGION:GUID.
mdiIdentityPoolId :: Lens' MergeDeveloperIdentities Text
mdiIdentityPoolId = lens _mdiIdentityPoolId (\ s a -> s{_mdiIdentityPoolId = a})

instance AWSRequest MergeDeveloperIdentities where
        type Rs MergeDeveloperIdentities =
             MergeDeveloperIdentitiesResponse
        request = postJSON cognitoIdentity
        response
          = receiveJSON
              (\ s h x ->
                 MergeDeveloperIdentitiesResponse' <$>
                   (x .?> "IdentityId") <*> (pure (fromEnum s)))

instance Hashable MergeDeveloperIdentities where

instance NFData MergeDeveloperIdentities where

instance ToHeaders MergeDeveloperIdentities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.MergeDeveloperIdentities"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON MergeDeveloperIdentities where
        toJSON MergeDeveloperIdentities'{..}
          = object
              (catMaybes
                 [Just
                    ("SourceUserIdentifier" .= _mdiSourceUserIdentifier),
                  Just
                    ("DestinationUserIdentifier" .=
                       _mdiDestinationUserIdentifier),
                  Just
                    ("DeveloperProviderName" .=
                       _mdiDeveloperProviderName),
                  Just ("IdentityPoolId" .= _mdiIdentityPoolId)])

instance ToPath MergeDeveloperIdentities where
        toPath = const "/"

instance ToQuery MergeDeveloperIdentities where
        toQuery = const mempty

-- | Returned in response to a successful @MergeDeveloperIdentities@ action.
--
--
--
-- /See:/ 'mergeDeveloperIdentitiesResponse' smart constructor.
data MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse'
  { _mdirsIdentityId     :: !(Maybe Text)
  , _mdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MergeDeveloperIdentitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdirsIdentityId' - A unique identifier in the format REGION:GUID.
--
-- * 'mdirsResponseStatus' - -- | The response status code.
mergeDeveloperIdentitiesResponse
    :: Int -- ^ 'mdirsResponseStatus'
    -> MergeDeveloperIdentitiesResponse
mergeDeveloperIdentitiesResponse pResponseStatus_ =
  MergeDeveloperIdentitiesResponse'
    {_mdirsIdentityId = Nothing, _mdirsResponseStatus = pResponseStatus_}


-- | A unique identifier in the format REGION:GUID.
mdirsIdentityId :: Lens' MergeDeveloperIdentitiesResponse (Maybe Text)
mdirsIdentityId = lens _mdirsIdentityId (\ s a -> s{_mdirsIdentityId = a})

-- | -- | The response status code.
mdirsResponseStatus :: Lens' MergeDeveloperIdentitiesResponse Int
mdirsResponseStatus = lens _mdirsResponseStatus (\ s a -> s{_mdirsResponseStatus = a})

instance NFData MergeDeveloperIdentitiesResponse
         where
