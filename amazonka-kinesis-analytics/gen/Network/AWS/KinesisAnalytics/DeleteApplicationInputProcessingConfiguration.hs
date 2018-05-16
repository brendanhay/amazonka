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
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an 'InputProcessingConfiguration' from an input.
--
--
module Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
    (
    -- * Creating a Request
      deleteApplicationInputProcessingConfiguration
    , DeleteApplicationInputProcessingConfiguration
    -- * Request Lenses
    , daipcApplicationName
    , daipcCurrentApplicationVersionId
    , daipcInputId

    -- * Destructuring the Response
    , deleteApplicationInputProcessingConfigurationResponse
    , DeleteApplicationInputProcessingConfigurationResponse
    -- * Response Lenses
    , daipcrsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApplicationInputProcessingConfiguration' smart constructor.
data DeleteApplicationInputProcessingConfiguration = DeleteApplicationInputProcessingConfiguration'
  { _daipcApplicationName             :: !Text
  , _daipcCurrentApplicationVersionId :: !Nat
  , _daipcInputId                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationInputProcessingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daipcApplicationName' - The Kinesis Analytics application name.
--
-- * 'daipcCurrentApplicationVersionId' - The version ID of the Kinesis Analytics application.
--
-- * 'daipcInputId' - The ID of the input configuration from which to delete the input processing configuration. You can get a list of the input IDs for an application by using the 'DescribeApplication' operation.
deleteApplicationInputProcessingConfiguration
    :: Text -- ^ 'daipcApplicationName'
    -> Natural -- ^ 'daipcCurrentApplicationVersionId'
    -> Text -- ^ 'daipcInputId'
    -> DeleteApplicationInputProcessingConfiguration
deleteApplicationInputProcessingConfiguration pApplicationName_ pCurrentApplicationVersionId_ pInputId_ =
  DeleteApplicationInputProcessingConfiguration'
    { _daipcApplicationName = pApplicationName_
    , _daipcCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _daipcInputId = pInputId_
    }


-- | The Kinesis Analytics application name.
daipcApplicationName :: Lens' DeleteApplicationInputProcessingConfiguration Text
daipcApplicationName = lens _daipcApplicationName (\ s a -> s{_daipcApplicationName = a})

-- | The version ID of the Kinesis Analytics application.
daipcCurrentApplicationVersionId :: Lens' DeleteApplicationInputProcessingConfiguration Natural
daipcCurrentApplicationVersionId = lens _daipcCurrentApplicationVersionId (\ s a -> s{_daipcCurrentApplicationVersionId = a}) . _Nat

-- | The ID of the input configuration from which to delete the input processing configuration. You can get a list of the input IDs for an application by using the 'DescribeApplication' operation.
daipcInputId :: Lens' DeleteApplicationInputProcessingConfiguration Text
daipcInputId = lens _daipcInputId (\ s a -> s{_daipcInputId = a})

instance AWSRequest
           DeleteApplicationInputProcessingConfiguration
         where
        type Rs DeleteApplicationInputProcessingConfiguration
             =
             DeleteApplicationInputProcessingConfigurationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteApplicationInputProcessingConfigurationResponse'
                   <$> (pure (fromEnum s)))

instance Hashable
           DeleteApplicationInputProcessingConfiguration
         where

instance NFData
           DeleteApplicationInputProcessingConfiguration
         where

instance ToHeaders
           DeleteApplicationInputProcessingConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.DeleteApplicationInputProcessingConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DeleteApplicationInputProcessingConfiguration
         where
        toJSON
          DeleteApplicationInputProcessingConfiguration'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _daipcApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _daipcCurrentApplicationVersionId),
                  Just ("InputId" .= _daipcInputId)])

instance ToPath
           DeleteApplicationInputProcessingConfiguration
         where
        toPath = const "/"

instance ToQuery
           DeleteApplicationInputProcessingConfiguration
         where
        toQuery = const mempty

-- | /See:/ 'deleteApplicationInputProcessingConfigurationResponse' smart constructor.
newtype DeleteApplicationInputProcessingConfigurationResponse = DeleteApplicationInputProcessingConfigurationResponse'
  { _daipcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationInputProcessingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daipcrsResponseStatus' - -- | The response status code.
deleteApplicationInputProcessingConfigurationResponse
    :: Int -- ^ 'daipcrsResponseStatus'
    -> DeleteApplicationInputProcessingConfigurationResponse
deleteApplicationInputProcessingConfigurationResponse pResponseStatus_ =
  DeleteApplicationInputProcessingConfigurationResponse'
    {_daipcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
daipcrsResponseStatus :: Lens' DeleteApplicationInputProcessingConfigurationResponse Int
daipcrsResponseStatus = lens _daipcrsResponseStatus (\ s a -> s{_daipcrsResponseStatus = a})

instance NFData
           DeleteApplicationInputProcessingConfigurationResponse
         where
