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
-- Module      : Network.AWS.IoT.SetDefaultAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the default authorizer. This will be used if a websocket connection is made without specifying an authorizer.
--
--
module Network.AWS.IoT.SetDefaultAuthorizer
    (
    -- * Creating a Request
      setDefaultAuthorizer
    , SetDefaultAuthorizer
    -- * Request Lenses
    , sdaAuthorizerName

    -- * Destructuring the Response
    , setDefaultAuthorizerResponse
    , SetDefaultAuthorizerResponse
    -- * Response Lenses
    , sdarsAuthorizerName
    , sdarsAuthorizerARN
    , sdarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setDefaultAuthorizer' smart constructor.
newtype SetDefaultAuthorizer = SetDefaultAuthorizer'
  { _sdaAuthorizerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDefaultAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdaAuthorizerName' - The authorizer name.
setDefaultAuthorizer
    :: Text -- ^ 'sdaAuthorizerName'
    -> SetDefaultAuthorizer
setDefaultAuthorizer pAuthorizerName_ =
  SetDefaultAuthorizer' {_sdaAuthorizerName = pAuthorizerName_}


-- | The authorizer name.
sdaAuthorizerName :: Lens' SetDefaultAuthorizer Text
sdaAuthorizerName = lens _sdaAuthorizerName (\ s a -> s{_sdaAuthorizerName = a})

instance AWSRequest SetDefaultAuthorizer where
        type Rs SetDefaultAuthorizer =
             SetDefaultAuthorizerResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 SetDefaultAuthorizerResponse' <$>
                   (x .?> "authorizerName") <*> (x .?> "authorizerArn")
                     <*> (pure (fromEnum s)))

instance Hashable SetDefaultAuthorizer where

instance NFData SetDefaultAuthorizer where

instance ToHeaders SetDefaultAuthorizer where
        toHeaders = const mempty

instance ToJSON SetDefaultAuthorizer where
        toJSON SetDefaultAuthorizer'{..}
          = object
              (catMaybes
                 [Just ("authorizerName" .= _sdaAuthorizerName)])

instance ToPath SetDefaultAuthorizer where
        toPath = const "/default-authorizer"

instance ToQuery SetDefaultAuthorizer where
        toQuery = const mempty

-- | /See:/ 'setDefaultAuthorizerResponse' smart constructor.
data SetDefaultAuthorizerResponse = SetDefaultAuthorizerResponse'
  { _sdarsAuthorizerName :: !(Maybe Text)
  , _sdarsAuthorizerARN  :: !(Maybe Text)
  , _sdarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDefaultAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdarsAuthorizerName' - The authorizer name.
--
-- * 'sdarsAuthorizerARN' - The authorizer ARN.
--
-- * 'sdarsResponseStatus' - -- | The response status code.
setDefaultAuthorizerResponse
    :: Int -- ^ 'sdarsResponseStatus'
    -> SetDefaultAuthorizerResponse
setDefaultAuthorizerResponse pResponseStatus_ =
  SetDefaultAuthorizerResponse'
    { _sdarsAuthorizerName = Nothing
    , _sdarsAuthorizerARN = Nothing
    , _sdarsResponseStatus = pResponseStatus_
    }


-- | The authorizer name.
sdarsAuthorizerName :: Lens' SetDefaultAuthorizerResponse (Maybe Text)
sdarsAuthorizerName = lens _sdarsAuthorizerName (\ s a -> s{_sdarsAuthorizerName = a})

-- | The authorizer ARN.
sdarsAuthorizerARN :: Lens' SetDefaultAuthorizerResponse (Maybe Text)
sdarsAuthorizerARN = lens _sdarsAuthorizerARN (\ s a -> s{_sdarsAuthorizerARN = a})

-- | -- | The response status code.
sdarsResponseStatus :: Lens' SetDefaultAuthorizerResponse Int
sdarsResponseStatus = lens _sdarsResponseStatus (\ s a -> s{_sdarsResponseStatus = a})

instance NFData SetDefaultAuthorizerResponse where
