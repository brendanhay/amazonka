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
-- Module      : Network.AWS.Firehose.Types.InputFormatConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.InputFormatConfiguration where

import Network.AWS.Firehose.Types.Deserializer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the deserializer you want to use to convert the format of the
-- input data. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'newInputFormatConfiguration' smart constructor.
data InputFormatConfiguration = InputFormatConfiguration'
  { -- | Specifies which deserializer to use. You can choose either the Apache
    -- Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the
    -- server rejects the request.
    deserializer :: Prelude.Maybe Deserializer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deserializer', 'inputFormatConfiguration_deserializer' - Specifies which deserializer to use. You can choose either the Apache
-- Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the
-- server rejects the request.
newInputFormatConfiguration ::
  InputFormatConfiguration
newInputFormatConfiguration =
  InputFormatConfiguration'
    { deserializer =
        Prelude.Nothing
    }

-- | Specifies which deserializer to use. You can choose either the Apache
-- Hive JSON SerDe or the OpenX JSON SerDe. If both are non-null, the
-- server rejects the request.
inputFormatConfiguration_deserializer :: Lens.Lens' InputFormatConfiguration (Prelude.Maybe Deserializer)
inputFormatConfiguration_deserializer = Lens.lens (\InputFormatConfiguration' {deserializer} -> deserializer) (\s@InputFormatConfiguration' {} a -> s {deserializer = a} :: InputFormatConfiguration)

instance Prelude.FromJSON InputFormatConfiguration where
  parseJSON =
    Prelude.withObject
      "InputFormatConfiguration"
      ( \x ->
          InputFormatConfiguration'
            Prelude.<$> (x Prelude..:? "Deserializer")
      )

instance Prelude.Hashable InputFormatConfiguration

instance Prelude.NFData InputFormatConfiguration

instance Prelude.ToJSON InputFormatConfiguration where
  toJSON InputFormatConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Deserializer" Prelude..=)
              Prelude.<$> deserializer
          ]
      )
