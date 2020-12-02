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
-- Module      : Network.AWS.Greengrass.CreateGroupCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CA for the group. If a CA already exists, it will rotate the existing CA.
module Network.AWS.Greengrass.CreateGroupCertificateAuthority
    (
    -- * Creating a Request
      createGroupCertificateAuthority
    , CreateGroupCertificateAuthority
    -- * Request Lenses
    , cgcaAmznClientToken
    , cgcaGroupId

    -- * Destructuring the Response
    , createGroupCertificateAuthorityResponse
    , CreateGroupCertificateAuthorityResponse
    -- * Response Lenses
    , cgcarsGroupCertificateAuthorityARN
    , cgcarsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGroupCertificateAuthority' smart constructor.
data CreateGroupCertificateAuthority = CreateGroupCertificateAuthority'
  { _cgcaAmznClientToken :: !(Maybe Text)
  , _cgcaGroupId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgcaAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cgcaGroupId' - The ID of the AWS Greengrass group.
createGroupCertificateAuthority
    :: Text -- ^ 'cgcaGroupId'
    -> CreateGroupCertificateAuthority
createGroupCertificateAuthority pGroupId_ =
  CreateGroupCertificateAuthority'
    {_cgcaAmznClientToken = Nothing, _cgcaGroupId = pGroupId_}


-- | A client token used to correlate requests and responses.
cgcaAmznClientToken :: Lens' CreateGroupCertificateAuthority (Maybe Text)
cgcaAmznClientToken = lens _cgcaAmznClientToken (\ s a -> s{_cgcaAmznClientToken = a})

-- | The ID of the AWS Greengrass group.
cgcaGroupId :: Lens' CreateGroupCertificateAuthority Text
cgcaGroupId = lens _cgcaGroupId (\ s a -> s{_cgcaGroupId = a})

instance AWSRequest CreateGroupCertificateAuthority
         where
        type Rs CreateGroupCertificateAuthority =
             CreateGroupCertificateAuthorityResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateGroupCertificateAuthorityResponse' <$>
                   (x .?> "GroupCertificateAuthorityArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateGroupCertificateAuthority
         where

instance NFData CreateGroupCertificateAuthority where

instance ToHeaders CreateGroupCertificateAuthority
         where
        toHeaders CreateGroupCertificateAuthority'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cgcaAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateGroupCertificateAuthority where
        toJSON = const (Object mempty)

instance ToPath CreateGroupCertificateAuthority where
        toPath CreateGroupCertificateAuthority'{..}
          = mconcat
              ["/greengrass/groups/", toBS _cgcaGroupId,
               "/certificateauthorities"]

instance ToQuery CreateGroupCertificateAuthority
         where
        toQuery = const mempty

-- | /See:/ 'createGroupCertificateAuthorityResponse' smart constructor.
data CreateGroupCertificateAuthorityResponse = CreateGroupCertificateAuthorityResponse'
  { _cgcarsGroupCertificateAuthorityARN :: !(Maybe Text)
  , _cgcarsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgcarsGroupCertificateAuthorityARN' - The ARN of the group certificate authority.
--
-- * 'cgcarsResponseStatus' - -- | The response status code.
createGroupCertificateAuthorityResponse
    :: Int -- ^ 'cgcarsResponseStatus'
    -> CreateGroupCertificateAuthorityResponse
createGroupCertificateAuthorityResponse pResponseStatus_ =
  CreateGroupCertificateAuthorityResponse'
    { _cgcarsGroupCertificateAuthorityARN = Nothing
    , _cgcarsResponseStatus = pResponseStatus_
    }


-- | The ARN of the group certificate authority.
cgcarsGroupCertificateAuthorityARN :: Lens' CreateGroupCertificateAuthorityResponse (Maybe Text)
cgcarsGroupCertificateAuthorityARN = lens _cgcarsGroupCertificateAuthorityARN (\ s a -> s{_cgcarsGroupCertificateAuthorityARN = a})

-- | -- | The response status code.
cgcarsResponseStatus :: Lens' CreateGroupCertificateAuthorityResponse Int
cgcarsResponseStatus = lens _cgcarsResponseStatus (\ s a -> s{_cgcarsResponseStatus = a})

instance NFData
           CreateGroupCertificateAuthorityResponse
         where
