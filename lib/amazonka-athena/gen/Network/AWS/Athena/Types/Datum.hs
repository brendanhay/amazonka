{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Datum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Datum where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A piece of data (a field in the table).
--
--
--
-- /See:/ 'datum' smart constructor.
newtype Datum = Datum' {_dVarCharValue :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Datum' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVarCharValue' - The value of the datum.
datum ::
  Datum
datum = Datum' {_dVarCharValue = Nothing}

-- | The value of the datum.
dVarCharValue :: Lens' Datum (Maybe Text)
dVarCharValue = lens _dVarCharValue (\s a -> s {_dVarCharValue = a})

instance FromJSON Datum where
  parseJSON =
    withObject "Datum" (\x -> Datum' <$> (x .:? "VarCharValue"))

instance Hashable Datum

instance NFData Datum
