{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilter where

import Network.AWS.CodeDeploy.Types.EC2TagFilterType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an EC2 tag filter.
--
--
--
-- /See:/ 'ec2TagFilter' smart constructor.
data EC2TagFilter = EC2TagFilter'
  { _etfValue :: !(Maybe Text),
    _etfKey :: !(Maybe Text),
    _etfType :: !(Maybe EC2TagFilterType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2TagFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etfValue' - The tag filter value.
--
-- * 'etfKey' - The tag filter key.
--
-- * 'etfType' - The tag filter type:     * @KEY_ONLY@ : Key only.     * @VALUE_ONLY@ : Value only.     * @KEY_AND_VALUE@ : Key and value.
ec2TagFilter ::
  EC2TagFilter
ec2TagFilter =
  EC2TagFilter'
    { _etfValue = Nothing,
      _etfKey = Nothing,
      _etfType = Nothing
    }

-- | The tag filter value.
etfValue :: Lens' EC2TagFilter (Maybe Text)
etfValue = lens _etfValue (\s a -> s {_etfValue = a})

-- | The tag filter key.
etfKey :: Lens' EC2TagFilter (Maybe Text)
etfKey = lens _etfKey (\s a -> s {_etfKey = a})

-- | The tag filter type:     * @KEY_ONLY@ : Key only.     * @VALUE_ONLY@ : Value only.     * @KEY_AND_VALUE@ : Key and value.
etfType :: Lens' EC2TagFilter (Maybe EC2TagFilterType)
etfType = lens _etfType (\s a -> s {_etfType = a})

instance FromJSON EC2TagFilter where
  parseJSON =
    withObject
      "EC2TagFilter"
      ( \x ->
          EC2TagFilter'
            <$> (x .:? "Value") <*> (x .:? "Key") <*> (x .:? "Type")
      )

instance Hashable EC2TagFilter

instance NFData EC2TagFilter

instance ToJSON EC2TagFilter where
  toJSON EC2TagFilter' {..} =
    object
      ( catMaybes
          [ ("Value" .=) <$> _etfValue,
            ("Key" .=) <$> _etfKey,
            ("Type" .=) <$> _etfType
          ]
      )
