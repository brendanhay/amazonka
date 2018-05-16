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
-- Module      : Network.AWS.STS.GetCallerIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the IAM identity whose credentials are used to call the API.
--
--
module Network.AWS.STS.GetCallerIdentity
    (
    -- * Creating a Request
      getCallerIdentity
    , GetCallerIdentity

    -- * Destructuring the Response
    , getCallerIdentityResponse
    , GetCallerIdentityResponse
    -- * Response Lenses
    , gcirsARN
    , gcirsAccount
    , gcirsUserId
    , gcirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types
import Network.AWS.STS.Types.Product

-- | /See:/ 'getCallerIdentity' smart constructor.
data GetCallerIdentity =
  GetCallerIdentity'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCallerIdentity' with the minimum fields required to make a request.
--
getCallerIdentity
    :: GetCallerIdentity
getCallerIdentity = GetCallerIdentity'


instance AWSRequest GetCallerIdentity where
        type Rs GetCallerIdentity = GetCallerIdentityResponse
        request = postQuery sts
        response
          = receiveXMLWrapper "GetCallerIdentityResult"
              (\ s h x ->
                 GetCallerIdentityResponse' <$>
                   (x .@? "Arn") <*> (x .@? "Account") <*>
                     (x .@? "UserId")
                     <*> (pure (fromEnum s)))

instance Hashable GetCallerIdentity where

instance NFData GetCallerIdentity where

instance ToHeaders GetCallerIdentity where
        toHeaders = const mempty

instance ToPath GetCallerIdentity where
        toPath = const "/"

instance ToQuery GetCallerIdentity where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("GetCallerIdentity" :: ByteString),
                  "Version" =: ("2011-06-15" :: ByteString)])

-- | Contains the response to a successful 'GetCallerIdentity' request, including information about the entity making the request.
--
--
--
-- /See:/ 'getCallerIdentityResponse' smart constructor.
data GetCallerIdentityResponse = GetCallerIdentityResponse'
  { _gcirsARN            :: !(Maybe Text)
  , _gcirsAccount        :: !(Maybe Text)
  , _gcirsUserId         :: !(Maybe Text)
  , _gcirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCallerIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirsARN' - The AWS ARN associated with the calling entity.
--
-- * 'gcirsAccount' - The AWS account ID number of the account that owns or contains the calling entity.
--
-- * 'gcirsUserId' - The unique identifier of the calling entity. The exact value depends on the type of entity making the call. The values returned are those listed in the __aws:userid__ column in the <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table> found on the __Policy Variables__ reference page in the /IAM User Guide/ .
--
-- * 'gcirsResponseStatus' - -- | The response status code.
getCallerIdentityResponse
    :: Int -- ^ 'gcirsResponseStatus'
    -> GetCallerIdentityResponse
getCallerIdentityResponse pResponseStatus_ =
  GetCallerIdentityResponse'
    { _gcirsARN = Nothing
    , _gcirsAccount = Nothing
    , _gcirsUserId = Nothing
    , _gcirsResponseStatus = pResponseStatus_
    }


-- | The AWS ARN associated with the calling entity.
gcirsARN :: Lens' GetCallerIdentityResponse (Maybe Text)
gcirsARN = lens _gcirsARN (\ s a -> s{_gcirsARN = a})

-- | The AWS account ID number of the account that owns or contains the calling entity.
gcirsAccount :: Lens' GetCallerIdentityResponse (Maybe Text)
gcirsAccount = lens _gcirsAccount (\ s a -> s{_gcirsAccount = a})

-- | The unique identifier of the calling entity. The exact value depends on the type of entity making the call. The values returned are those listed in the __aws:userid__ column in the <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table> found on the __Policy Variables__ reference page in the /IAM User Guide/ .
gcirsUserId :: Lens' GetCallerIdentityResponse (Maybe Text)
gcirsUserId = lens _gcirsUserId (\ s a -> s{_gcirsUserId = a})

-- | -- | The response status code.
gcirsResponseStatus :: Lens' GetCallerIdentityResponse Int
gcirsResponseStatus = lens _gcirsResponseStatus (\ s a -> s{_gcirsResponseStatus = a})

instance NFData GetCallerIdentityResponse where
