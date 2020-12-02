{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLChannel where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLDataSource
import Network.AWS.SageMaker.Types.CompressionType

-- | Similar to Channel. A channel is a named input source that training algorithms can consume. Refer to Channel for detailed descriptions.
--
--
--
-- /See:/ 'autoMLChannel' smart constructor.
data AutoMLChannel = AutoMLChannel'
  { _amlcCompressionType ::
      !(Maybe CompressionType),
    _amlcDataSource :: !AutoMLDataSource,
    _amlcTargetAttributeName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlcCompressionType' - You can use Gzip or None. The default value is None.
--
-- * 'amlcDataSource' - The data source.
--
-- * 'amlcTargetAttributeName' - The name of the target variable in supervised learning, a.k.a. 'y'.
autoMLChannel ::
  -- | 'amlcDataSource'
  AutoMLDataSource ->
  -- | 'amlcTargetAttributeName'
  Text ->
  AutoMLChannel
autoMLChannel pDataSource_ pTargetAttributeName_ =
  AutoMLChannel'
    { _amlcCompressionType = Nothing,
      _amlcDataSource = pDataSource_,
      _amlcTargetAttributeName = pTargetAttributeName_
    }

-- | You can use Gzip or None. The default value is None.
amlcCompressionType :: Lens' AutoMLChannel (Maybe CompressionType)
amlcCompressionType = lens _amlcCompressionType (\s a -> s {_amlcCompressionType = a})

-- | The data source.
amlcDataSource :: Lens' AutoMLChannel AutoMLDataSource
amlcDataSource = lens _amlcDataSource (\s a -> s {_amlcDataSource = a})

-- | The name of the target variable in supervised learning, a.k.a. 'y'.
amlcTargetAttributeName :: Lens' AutoMLChannel Text
amlcTargetAttributeName = lens _amlcTargetAttributeName (\s a -> s {_amlcTargetAttributeName = a})

instance FromJSON AutoMLChannel where
  parseJSON =
    withObject
      "AutoMLChannel"
      ( \x ->
          AutoMLChannel'
            <$> (x .:? "CompressionType")
            <*> (x .: "DataSource")
            <*> (x .: "TargetAttributeName")
      )

instance Hashable AutoMLChannel

instance NFData AutoMLChannel

instance ToJSON AutoMLChannel where
  toJSON AutoMLChannel' {..} =
    object
      ( catMaybes
          [ ("CompressionType" .=) <$> _amlcCompressionType,
            Just ("DataSource" .= _amlcDataSource),
            Just ("TargetAttributeName" .= _amlcTargetAttributeName)
          ]
      )
