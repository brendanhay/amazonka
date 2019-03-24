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
-- Module      : Network.AWS.Translate.GetTerminology
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom terminology.
--
--
module Network.AWS.Translate.GetTerminology
    (
    -- * Creating a Request
      getTerminology
    , GetTerminology
    -- * Request Lenses
    , gtName
    , gtTerminologyDataFormat

    -- * Destructuring the Response
    , getTerminologyResponse
    , GetTerminologyResponse
    -- * Response Lenses
    , gtrsTerminologyProperties
    , gtrsTerminologyDataLocation
    , gtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types
import Network.AWS.Translate.Types.Product

-- | /See:/ 'getTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { _gtName                  :: !Text
  , _gtTerminologyDataFormat :: !TerminologyDataFormat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTerminology' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtName' - The name of the custom terminology being retrieved.
--
-- * 'gtTerminologyDataFormat' - The data format of the custom terminology being retrieved, either CSV or TMX.
getTerminology
    :: Text -- ^ 'gtName'
    -> TerminologyDataFormat -- ^ 'gtTerminologyDataFormat'
    -> GetTerminology
getTerminology pName_ pTerminologyDataFormat_ =
  GetTerminology'
    {_gtName = pName_, _gtTerminologyDataFormat = pTerminologyDataFormat_}


-- | The name of the custom terminology being retrieved.
gtName :: Lens' GetTerminology Text
gtName = lens _gtName (\ s a -> s{_gtName = a})

-- | The data format of the custom terminology being retrieved, either CSV or TMX.
gtTerminologyDataFormat :: Lens' GetTerminology TerminologyDataFormat
gtTerminologyDataFormat = lens _gtTerminologyDataFormat (\ s a -> s{_gtTerminologyDataFormat = a})

instance AWSRequest GetTerminology where
        type Rs GetTerminology = GetTerminologyResponse
        request = postJSON translate
        response
          = receiveJSON
              (\ s h x ->
                 GetTerminologyResponse' <$>
                   (x .?> "TerminologyProperties") <*>
                     (x .?> "TerminologyDataLocation")
                     <*> (pure (fromEnum s)))

instance Hashable GetTerminology where

instance NFData GetTerminology where

instance ToHeaders GetTerminology where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShineFrontendService_20170701.GetTerminology" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetTerminology where
        toJSON GetTerminology'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _gtName),
                  Just
                    ("TerminologyDataFormat" .=
                       _gtTerminologyDataFormat)])

instance ToPath GetTerminology where
        toPath = const "/"

instance ToQuery GetTerminology where
        toQuery = const mempty

-- | /See:/ 'getTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { _gtrsTerminologyProperties   :: !(Maybe TerminologyProperties)
  , _gtrsTerminologyDataLocation :: !(Maybe TerminologyDataLocation)
  , _gtrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTerminologyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrsTerminologyProperties' - The properties of the custom terminology being retrieved.
--
-- * 'gtrsTerminologyDataLocation' - The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
--
-- * 'gtrsResponseStatus' - -- | The response status code.
getTerminologyResponse
    :: Int -- ^ 'gtrsResponseStatus'
    -> GetTerminologyResponse
getTerminologyResponse pResponseStatus_ =
  GetTerminologyResponse'
    { _gtrsTerminologyProperties = Nothing
    , _gtrsTerminologyDataLocation = Nothing
    , _gtrsResponseStatus = pResponseStatus_
    }


-- | The properties of the custom terminology being retrieved.
gtrsTerminologyProperties :: Lens' GetTerminologyResponse (Maybe TerminologyProperties)
gtrsTerminologyProperties = lens _gtrsTerminologyProperties (\ s a -> s{_gtrsTerminologyProperties = a})

-- | The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
gtrsTerminologyDataLocation :: Lens' GetTerminologyResponse (Maybe TerminologyDataLocation)
gtrsTerminologyDataLocation = lens _gtrsTerminologyDataLocation (\ s a -> s{_gtrsTerminologyDataLocation = a})

-- | -- | The response status code.
gtrsResponseStatus :: Lens' GetTerminologyResponse Int
gtrsResponseStatus = lens _gtrsResponseStatus (\ s a -> s{_gtrsResponseStatus = a})

instance NFData GetTerminologyResponse where
