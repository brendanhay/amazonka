{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.AvailableProcessorFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AvailableProcessorFeature where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the available processor feature information for the DB instance class of a DB instance.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class> in the /Amazon RDS User Guide. /
--
--
-- /See:/ 'availableProcessorFeature' smart constructor.
data AvailableProcessorFeature = AvailableProcessorFeature'
  { _apfName ::
      !(Maybe Text),
    _apfDefaultValue :: !(Maybe Text),
    _apfAllowedValues :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailableProcessorFeature' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apfName' - The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
--
-- * 'apfDefaultValue' - The default value for the processor feature of the DB instance class.
--
-- * 'apfAllowedValues' - The allowed values for the processor feature of the DB instance class.
availableProcessorFeature ::
  AvailableProcessorFeature
availableProcessorFeature =
  AvailableProcessorFeature'
    { _apfName = Nothing,
      _apfDefaultValue = Nothing,
      _apfAllowedValues = Nothing
    }

-- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
apfName :: Lens' AvailableProcessorFeature (Maybe Text)
apfName = lens _apfName (\s a -> s {_apfName = a})

-- | The default value for the processor feature of the DB instance class.
apfDefaultValue :: Lens' AvailableProcessorFeature (Maybe Text)
apfDefaultValue = lens _apfDefaultValue (\s a -> s {_apfDefaultValue = a})

-- | The allowed values for the processor feature of the DB instance class.
apfAllowedValues :: Lens' AvailableProcessorFeature (Maybe Text)
apfAllowedValues = lens _apfAllowedValues (\s a -> s {_apfAllowedValues = a})

instance FromXML AvailableProcessorFeature where
  parseXML x =
    AvailableProcessorFeature'
      <$> (x .@? "Name")
      <*> (x .@? "DefaultValue")
      <*> (x .@? "AllowedValues")

instance Hashable AvailableProcessorFeature

instance NFData AvailableProcessorFeature
