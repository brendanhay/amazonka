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
-- Module      : Amazonka.Omics.Types.SseConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.SseConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Server-side encryption (SSE) settings for a store.
--
-- /See:/ 'newSseConfig' smart constructor.
data SseConfig = SseConfig'
  { -- | An encryption key ARN.
    keyArn :: Prelude.Maybe Prelude.Text,
    -- | The encryption type.
    type' :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SseConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'sseConfig_keyArn' - An encryption key ARN.
--
-- 'type'', 'sseConfig_type' - The encryption type.
newSseConfig ::
  -- | 'type''
  EncryptionType ->
  SseConfig
newSseConfig pType_ =
  SseConfig'
    { keyArn = Prelude.Nothing,
      type' = pType_
    }

-- | An encryption key ARN.
sseConfig_keyArn :: Lens.Lens' SseConfig (Prelude.Maybe Prelude.Text)
sseConfig_keyArn = Lens.lens (\SseConfig' {keyArn} -> keyArn) (\s@SseConfig' {} a -> s {keyArn = a} :: SseConfig)

-- | The encryption type.
sseConfig_type :: Lens.Lens' SseConfig EncryptionType
sseConfig_type = Lens.lens (\SseConfig' {type'} -> type') (\s@SseConfig' {} a -> s {type' = a} :: SseConfig)

instance Data.FromJSON SseConfig where
  parseJSON =
    Data.withObject
      "SseConfig"
      ( \x ->
          SseConfig'
            Prelude.<$> (x Data..:? "keyArn")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable SseConfig where
  hashWithSalt _salt SseConfig' {..} =
    _salt
      `Prelude.hashWithSalt` keyArn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SseConfig where
  rnf SseConfig' {..} =
    Prelude.rnf keyArn `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON SseConfig where
  toJSON SseConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("keyArn" Data..=) Prelude.<$> keyArn,
            Prelude.Just ("type" Data..= type')
          ]
      )
