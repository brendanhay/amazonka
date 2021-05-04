{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ClientProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.ReconnectEnum

-- | Describes an Amazon WorkSpaces client.
--
-- /See:/ 'newClientProperties' smart constructor.
data ClientProperties = ClientProperties'
  { -- | Specifies whether users can cache their credentials on the Amazon
    -- WorkSpaces client. When enabled, users can choose to reconnect to their
    -- WorkSpaces without re-entering their credentials.
    reconnectEnabled :: Prelude.Maybe ReconnectEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClientProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reconnectEnabled', 'clientProperties_reconnectEnabled' - Specifies whether users can cache their credentials on the Amazon
-- WorkSpaces client. When enabled, users can choose to reconnect to their
-- WorkSpaces without re-entering their credentials.
newClientProperties ::
  ClientProperties
newClientProperties =
  ClientProperties'
    { reconnectEnabled =
        Prelude.Nothing
    }

-- | Specifies whether users can cache their credentials on the Amazon
-- WorkSpaces client. When enabled, users can choose to reconnect to their
-- WorkSpaces without re-entering their credentials.
clientProperties_reconnectEnabled :: Lens.Lens' ClientProperties (Prelude.Maybe ReconnectEnum)
clientProperties_reconnectEnabled = Lens.lens (\ClientProperties' {reconnectEnabled} -> reconnectEnabled) (\s@ClientProperties' {} a -> s {reconnectEnabled = a} :: ClientProperties)

instance Prelude.FromJSON ClientProperties where
  parseJSON =
    Prelude.withObject
      "ClientProperties"
      ( \x ->
          ClientProperties'
            Prelude.<$> (x Prelude..:? "ReconnectEnabled")
      )

instance Prelude.Hashable ClientProperties

instance Prelude.NFData ClientProperties

instance Prelude.ToJSON ClientProperties where
  toJSON ClientProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReconnectEnabled" Prelude..=)
              Prelude.<$> reconnectEnabled
          ]
      )
