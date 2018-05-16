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
-- Module      : Network.AWS.CloudDirectory.GetAppliedSchemaVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns current applied schema version ARN, including the minor version in use.
--
--
module Network.AWS.CloudDirectory.GetAppliedSchemaVersion
    (
    -- * Creating a Request
      getAppliedSchemaVersion
    , GetAppliedSchemaVersion
    -- * Request Lenses
    , gasvSchemaARN

    -- * Destructuring the Response
    , getAppliedSchemaVersionResponse
    , GetAppliedSchemaVersionResponse
    -- * Response Lenses
    , gasvrsAppliedSchemaARN
    , gasvrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAppliedSchemaVersion' smart constructor.
newtype GetAppliedSchemaVersion = GetAppliedSchemaVersion'
  { _gasvSchemaARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppliedSchemaVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasvSchemaARN' - The ARN of the applied schema.
getAppliedSchemaVersion
    :: Text -- ^ 'gasvSchemaARN'
    -> GetAppliedSchemaVersion
getAppliedSchemaVersion pSchemaARN_ =
  GetAppliedSchemaVersion' {_gasvSchemaARN = pSchemaARN_}


-- | The ARN of the applied schema.
gasvSchemaARN :: Lens' GetAppliedSchemaVersion Text
gasvSchemaARN = lens _gasvSchemaARN (\ s a -> s{_gasvSchemaARN = a})

instance AWSRequest GetAppliedSchemaVersion where
        type Rs GetAppliedSchemaVersion =
             GetAppliedSchemaVersionResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 GetAppliedSchemaVersionResponse' <$>
                   (x .?> "AppliedSchemaArn") <*> (pure (fromEnum s)))

instance Hashable GetAppliedSchemaVersion where

instance NFData GetAppliedSchemaVersion where

instance ToHeaders GetAppliedSchemaVersion where
        toHeaders = const mempty

instance ToJSON GetAppliedSchemaVersion where
        toJSON GetAppliedSchemaVersion'{..}
          = object
              (catMaybes [Just ("SchemaArn" .= _gasvSchemaARN)])

instance ToPath GetAppliedSchemaVersion where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/getappliedschema"

instance ToQuery GetAppliedSchemaVersion where
        toQuery = const mempty

-- | /See:/ 'getAppliedSchemaVersionResponse' smart constructor.
data GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse'
  { _gasvrsAppliedSchemaARN :: !(Maybe Text)
  , _gasvrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppliedSchemaVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasvrsAppliedSchemaARN' - Current applied schema ARN, including the minor version in use if one was provided.
--
-- * 'gasvrsResponseStatus' - -- | The response status code.
getAppliedSchemaVersionResponse
    :: Int -- ^ 'gasvrsResponseStatus'
    -> GetAppliedSchemaVersionResponse
getAppliedSchemaVersionResponse pResponseStatus_ =
  GetAppliedSchemaVersionResponse'
    { _gasvrsAppliedSchemaARN = Nothing
    , _gasvrsResponseStatus = pResponseStatus_
    }


-- | Current applied schema ARN, including the minor version in use if one was provided.
gasvrsAppliedSchemaARN :: Lens' GetAppliedSchemaVersionResponse (Maybe Text)
gasvrsAppliedSchemaARN = lens _gasvrsAppliedSchemaARN (\ s a -> s{_gasvrsAppliedSchemaARN = a})

-- | -- | The response status code.
gasvrsResponseStatus :: Lens' GetAppliedSchemaVersionResponse Int
gasvrsResponseStatus = lens _gasvrsResponseStatus (\ s a -> s{_gasvrsResponseStatus = a})

instance NFData GetAppliedSchemaVersionResponse where
