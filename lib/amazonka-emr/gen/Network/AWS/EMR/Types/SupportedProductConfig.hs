{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SupportedProductConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SupportedProductConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The list of supported product configurations which allow user-supplied arguments. EMR accepts these arguments and forwards them to the corresponding installation script as bootstrap action arguments.
--
--
--
-- /See:/ 'supportedProductConfig' smart constructor.
data SupportedProductConfig = SupportedProductConfig'
  { _spcArgs ::
      !(Maybe [Text]),
    _spcName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SupportedProductConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spcArgs' - The list of user-supplied arguments.
--
-- * 'spcName' - The name of the product configuration.
supportedProductConfig ::
  SupportedProductConfig
supportedProductConfig =
  SupportedProductConfig' {_spcArgs = Nothing, _spcName = Nothing}

-- | The list of user-supplied arguments.
spcArgs :: Lens' SupportedProductConfig [Text]
spcArgs = lens _spcArgs (\s a -> s {_spcArgs = a}) . _Default . _Coerce

-- | The name of the product configuration.
spcName :: Lens' SupportedProductConfig (Maybe Text)
spcName = lens _spcName (\s a -> s {_spcName = a})

instance Hashable SupportedProductConfig

instance NFData SupportedProductConfig

instance ToJSON SupportedProductConfig where
  toJSON SupportedProductConfig' {..} =
    object
      (catMaybes [("Args" .=) <$> _spcArgs, ("Name" .=) <$> _spcName])
