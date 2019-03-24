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
-- Module      : Network.AWS.Config.PutRetentionConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the retention configuration with details about retention period (number of days) that AWS Config stores your historical information. The API creates the @RetentionConfiguration@ object and names the object as __default__ . When you have a @RetentionConfiguration@ object named __default__ , calling the API modifies the default object.
--
--
module Network.AWS.Config.PutRetentionConfiguration
    (
    -- * Creating a Request
      putRetentionConfiguration
    , PutRetentionConfiguration
    -- * Request Lenses
    , prcRetentionPeriodInDays

    -- * Destructuring the Response
    , putRetentionConfigurationResponse
    , PutRetentionConfigurationResponse
    -- * Response Lenses
    , prsRetentionConfiguration
    , prsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRetentionConfiguration' smart constructor.
newtype PutRetentionConfiguration = PutRetentionConfiguration'
  { _prcRetentionPeriodInDays :: Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRetentionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prcRetentionPeriodInDays' - Number of days AWS Config stores your historical information.
putRetentionConfiguration
    :: Natural -- ^ 'prcRetentionPeriodInDays'
    -> PutRetentionConfiguration
putRetentionConfiguration pRetentionPeriodInDays_ =
  PutRetentionConfiguration'
    {_prcRetentionPeriodInDays = _Nat # pRetentionPeriodInDays_}


-- | Number of days AWS Config stores your historical information.
prcRetentionPeriodInDays :: Lens' PutRetentionConfiguration Natural
prcRetentionPeriodInDays = lens _prcRetentionPeriodInDays (\ s a -> s{_prcRetentionPeriodInDays = a}) . _Nat

instance AWSRequest PutRetentionConfiguration where
        type Rs PutRetentionConfiguration =
             PutRetentionConfigurationResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 PutRetentionConfigurationResponse' <$>
                   (x .?> "RetentionConfiguration") <*>
                     (pure (fromEnum s)))

instance Hashable PutRetentionConfiguration where

instance NFData PutRetentionConfiguration where

instance ToHeaders PutRetentionConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.PutRetentionConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRetentionConfiguration where
        toJSON PutRetentionConfiguration'{..}
          = object
              (catMaybes
                 [Just
                    ("RetentionPeriodInDays" .=
                       _prcRetentionPeriodInDays)])

instance ToPath PutRetentionConfiguration where
        toPath = const "/"

instance ToQuery PutRetentionConfiguration where
        toQuery = const mempty

-- | /See:/ 'putRetentionConfigurationResponse' smart constructor.
data PutRetentionConfigurationResponse = PutRetentionConfigurationResponse'
  { _prsRetentionConfiguration :: !(Maybe RetentionConfiguration)
  , _prsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRetentionConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsRetentionConfiguration' - Returns a retention configuration object.
--
-- * 'prsResponseStatus' - -- | The response status code.
putRetentionConfigurationResponse
    :: Int -- ^ 'prsResponseStatus'
    -> PutRetentionConfigurationResponse
putRetentionConfigurationResponse pResponseStatus_ =
  PutRetentionConfigurationResponse'
    { _prsRetentionConfiguration = Nothing
    , _prsResponseStatus = pResponseStatus_
    }


-- | Returns a retention configuration object.
prsRetentionConfiguration :: Lens' PutRetentionConfigurationResponse (Maybe RetentionConfiguration)
prsRetentionConfiguration = lens _prsRetentionConfiguration (\ s a -> s{_prsRetentionConfiguration = a})

-- | -- | The response status code.
prsResponseStatus :: Lens' PutRetentionConfigurationResponse Int
prsResponseStatus = lens _prsResponseStatus (\ s a -> s{_prsResponseStatus = a})

instance NFData PutRetentionConfigurationResponse
         where
