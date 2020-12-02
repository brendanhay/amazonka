{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputConfiguration where

import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | When you start your application, you provide this configuration, which identifies the input source and the point in the input source at which you want the application to start processing records.
--
--
--
-- /See:/ 'inputConfiguration' smart constructor.
data InputConfiguration = InputConfiguration'
  { _icId :: !Text,
    _icInputStartingPositionConfiguration ::
      !InputStartingPositionConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icId' - Input source ID. You can get this ID by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- * 'icInputStartingPositionConfiguration' - Point at which you want the application to start processing records from the streaming source.
inputConfiguration ::
  -- | 'icId'
  Text ->
  -- | 'icInputStartingPositionConfiguration'
  InputStartingPositionConfiguration ->
  InputConfiguration
inputConfiguration pId_ pInputStartingPositionConfiguration_ =
  InputConfiguration'
    { _icId = pId_,
      _icInputStartingPositionConfiguration =
        pInputStartingPositionConfiguration_
    }

-- | Input source ID. You can get this ID by calling the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
icId :: Lens' InputConfiguration Text
icId = lens _icId (\s a -> s {_icId = a})

-- | Point at which you want the application to start processing records from the streaming source.
icInputStartingPositionConfiguration :: Lens' InputConfiguration InputStartingPositionConfiguration
icInputStartingPositionConfiguration = lens _icInputStartingPositionConfiguration (\s a -> s {_icInputStartingPositionConfiguration = a})

instance Hashable InputConfiguration

instance NFData InputConfiguration

instance ToJSON InputConfiguration where
  toJSON InputConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("Id" .= _icId),
            Just
              ( "InputStartingPositionConfiguration"
                  .= _icInputStartingPositionConfiguration
              )
          ]
      )
