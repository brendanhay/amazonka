{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a @DeveloperUserIdentifier@ from an existing identity. Unlinked
-- developer users will be considered new identities next time they are
-- seen. If, for a given Cognito identity, you remove all federated
-- identities as well as the developer user identifier, the Cognito
-- identity becomes inaccessible.
--
-- This is a public API. You do not need any credentials to call this API.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_UnlinkDeveloperIdentity.html>
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
    (
    -- * Request
      UnlinkDeveloperIdentity
    -- ** Request constructor
    , unlinkDeveloperIdentity
    -- ** Request lenses
    , udirqIdentityId
    , udirqIdentityPoolId
    , udirqDeveloperProviderName
    , udirqDeveloperUserIdentifier

    -- * Response
    , UnlinkDeveloperIdentityResponse
    -- ** Response constructor
    , unlinkDeveloperIdentityResponse
    ) where

import           Network.AWS.CognitoIdentity.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to the @UnlinkDeveloperIdentity@ action.
--
-- /See:/ 'unlinkDeveloperIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udirqIdentityId'
--
-- * 'udirqIdentityPoolId'
--
-- * 'udirqDeveloperProviderName'
--
-- * 'udirqDeveloperUserIdentifier'
data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity'
    { _udirqIdentityId              :: !Text
    , _udirqIdentityPoolId          :: !Text
    , _udirqDeveloperProviderName   :: !Text
    , _udirqDeveloperUserIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnlinkDeveloperIdentity' smart constructor.
unlinkDeveloperIdentity :: Text -> Text -> Text -> Text -> UnlinkDeveloperIdentity
unlinkDeveloperIdentity pIdentityId_ pIdentityPoolId_ pDeveloperProviderName_ pDeveloperUserIdentifier_ =
    UnlinkDeveloperIdentity'
    { _udirqIdentityId = pIdentityId_
    , _udirqIdentityPoolId = pIdentityPoolId_
    , _udirqDeveloperProviderName = pDeveloperProviderName_
    , _udirqDeveloperUserIdentifier = pDeveloperUserIdentifier_
    }

-- | A unique identifier in the format REGION:GUID.
udirqIdentityId :: Lens' UnlinkDeveloperIdentity Text
udirqIdentityId = lens _udirqIdentityId (\ s a -> s{_udirqIdentityId = a});

-- | An identity pool ID in the format REGION:GUID.
udirqIdentityPoolId :: Lens' UnlinkDeveloperIdentity Text
udirqIdentityPoolId = lens _udirqIdentityPoolId (\ s a -> s{_udirqIdentityPoolId = a});

-- | The \"domain\" by which Cognito will refer to your users.
udirqDeveloperProviderName :: Lens' UnlinkDeveloperIdentity Text
udirqDeveloperProviderName = lens _udirqDeveloperProviderName (\ s a -> s{_udirqDeveloperProviderName = a});

-- | A unique ID used by your backend authentication process to identify a
-- user.
udirqDeveloperUserIdentifier :: Lens' UnlinkDeveloperIdentity Text
udirqDeveloperUserIdentifier = lens _udirqDeveloperUserIdentifier (\ s a -> s{_udirqDeveloperUserIdentifier = a});

instance AWSRequest UnlinkDeveloperIdentity where
        type Sv UnlinkDeveloperIdentity = CognitoIdentity
        type Rs UnlinkDeveloperIdentity =
             UnlinkDeveloperIdentityResponse
        request = postJSON
        response
          = receiveNull UnlinkDeveloperIdentityResponse'

instance ToHeaders UnlinkDeveloperIdentity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityService.UnlinkDeveloperIdentity"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnlinkDeveloperIdentity where
        toJSON UnlinkDeveloperIdentity'{..}
          = object
              ["IdentityId" .= _udirqIdentityId,
               "IdentityPoolId" .= _udirqIdentityPoolId,
               "DeveloperProviderName" .=
                 _udirqDeveloperProviderName,
               "DeveloperUserIdentifier" .=
                 _udirqDeveloperUserIdentifier]

instance ToPath UnlinkDeveloperIdentity where
        toPath = const "/"

instance ToQuery UnlinkDeveloperIdentity where
        toQuery = const mempty

-- | /See:/ 'unlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse =
    UnlinkDeveloperIdentityResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnlinkDeveloperIdentityResponse' smart constructor.
unlinkDeveloperIdentityResponse :: UnlinkDeveloperIdentityResponse
unlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse'
