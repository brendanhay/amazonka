{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A specification identifying an individual configuration option.
--
--
--
-- /See:/ 'optionSpecification' smart constructor.
data OptionSpecification = OptionSpecification'
  { _osOptionName ::
      !(Maybe Text),
    _osResourceName :: !(Maybe Text),
    _osNamespace :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOptionName' - The name of the configuration option.
--
-- * 'osResourceName' - A unique resource name for a time-based scaling configuration option.
--
-- * 'osNamespace' - A unique namespace identifying the option's associated AWS resource.
optionSpecification ::
  OptionSpecification
optionSpecification =
  OptionSpecification'
    { _osOptionName = Nothing,
      _osResourceName = Nothing,
      _osNamespace = Nothing
    }

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\s a -> s {_osOptionName = a})

-- | A unique resource name for a time-based scaling configuration option.
osResourceName :: Lens' OptionSpecification (Maybe Text)
osResourceName = lens _osResourceName (\s a -> s {_osResourceName = a})

-- | A unique namespace identifying the option's associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\s a -> s {_osNamespace = a})

instance Hashable OptionSpecification

instance NFData OptionSpecification

instance ToQuery OptionSpecification where
  toQuery OptionSpecification' {..} =
    mconcat
      [ "OptionName" =: _osOptionName,
        "ResourceName" =: _osResourceName,
        "Namespace" =: _osNamespace
      ]
