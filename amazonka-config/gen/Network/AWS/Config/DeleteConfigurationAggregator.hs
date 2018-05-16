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
-- Module      : Network.AWS.Config.DeleteConfigurationAggregator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified configuration aggregator and the aggregated data associated with the aggregator.
--
--
module Network.AWS.Config.DeleteConfigurationAggregator
    (
    -- * Creating a Request
      deleteConfigurationAggregator
    , DeleteConfigurationAggregator
    -- * Request Lenses
    , dcaConfigurationAggregatorName

    -- * Destructuring the Response
    , deleteConfigurationAggregatorResponse
    , DeleteConfigurationAggregatorResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConfigurationAggregator' smart constructor.
newtype DeleteConfigurationAggregator = DeleteConfigurationAggregator'
  { _dcaConfigurationAggregatorName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationAggregator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaConfigurationAggregatorName' - The name of the configuration aggregator.
deleteConfigurationAggregator
    :: Text -- ^ 'dcaConfigurationAggregatorName'
    -> DeleteConfigurationAggregator
deleteConfigurationAggregator pConfigurationAggregatorName_ =
  DeleteConfigurationAggregator'
    {_dcaConfigurationAggregatorName = pConfigurationAggregatorName_}


-- | The name of the configuration aggregator.
dcaConfigurationAggregatorName :: Lens' DeleteConfigurationAggregator Text
dcaConfigurationAggregatorName = lens _dcaConfigurationAggregatorName (\ s a -> s{_dcaConfigurationAggregatorName = a})

instance AWSRequest DeleteConfigurationAggregator
         where
        type Rs DeleteConfigurationAggregator =
             DeleteConfigurationAggregatorResponse
        request = postJSON config
        response
          = receiveNull DeleteConfigurationAggregatorResponse'

instance Hashable DeleteConfigurationAggregator where

instance NFData DeleteConfigurationAggregator where

instance ToHeaders DeleteConfigurationAggregator
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteConfigurationAggregator"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteConfigurationAggregator where
        toJSON DeleteConfigurationAggregator'{..}
          = object
              (catMaybes
                 [Just
                    ("ConfigurationAggregatorName" .=
                       _dcaConfigurationAggregatorName)])

instance ToPath DeleteConfigurationAggregator where
        toPath = const "/"

instance ToQuery DeleteConfigurationAggregator where
        toQuery = const mempty

-- | /See:/ 'deleteConfigurationAggregatorResponse' smart constructor.
data DeleteConfigurationAggregatorResponse =
  DeleteConfigurationAggregatorResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationAggregatorResponse' with the minimum fields required to make a request.
--
deleteConfigurationAggregatorResponse
    :: DeleteConfigurationAggregatorResponse
deleteConfigurationAggregatorResponse = DeleteConfigurationAggregatorResponse'


instance NFData DeleteConfigurationAggregatorResponse
         where
