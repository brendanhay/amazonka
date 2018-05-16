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
-- Module      : Network.AWS.IoTAnalytics.CreateDatasetContent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the content of a data set by applying an SQL action.
--
--
module Network.AWS.IoTAnalytics.CreateDatasetContent
    (
    -- * Creating a Request
      createDatasetContent
    , CreateDatasetContent
    -- * Request Lenses
    , cdcDatasetName

    -- * Destructuring the Response
    , createDatasetContentResponse
    , CreateDatasetContentResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDatasetContent' smart constructor.
newtype CreateDatasetContent = CreateDatasetContent'
  { _cdcDatasetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatasetContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcDatasetName' - The name of the data set.
createDatasetContent
    :: Text -- ^ 'cdcDatasetName'
    -> CreateDatasetContent
createDatasetContent pDatasetName_ =
  CreateDatasetContent' {_cdcDatasetName = pDatasetName_}


-- | The name of the data set.
cdcDatasetName :: Lens' CreateDatasetContent Text
cdcDatasetName = lens _cdcDatasetName (\ s a -> s{_cdcDatasetName = a})

instance AWSRequest CreateDatasetContent where
        type Rs CreateDatasetContent =
             CreateDatasetContentResponse
        request = postJSON ioTAnalytics
        response = receiveNull CreateDatasetContentResponse'

instance Hashable CreateDatasetContent where

instance NFData CreateDatasetContent where

instance ToHeaders CreateDatasetContent where
        toHeaders = const mempty

instance ToJSON CreateDatasetContent where
        toJSON = const (Object mempty)

instance ToPath CreateDatasetContent where
        toPath CreateDatasetContent'{..}
          = mconcat
              ["/datasets/", toBS _cdcDatasetName, "/content"]

instance ToQuery CreateDatasetContent where
        toQuery = const mempty

-- | /See:/ 'createDatasetContentResponse' smart constructor.
data CreateDatasetContentResponse =
  CreateDatasetContentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDatasetContentResponse' with the minimum fields required to make a request.
--
createDatasetContentResponse
    :: CreateDatasetContentResponse
createDatasetContentResponse = CreateDatasetContentResponse'


instance NFData CreateDatasetContentResponse where
