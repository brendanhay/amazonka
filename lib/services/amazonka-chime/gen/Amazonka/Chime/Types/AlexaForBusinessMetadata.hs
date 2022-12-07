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
-- Module      : Amazonka.Chime.Types.AlexaForBusinessMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AlexaForBusinessMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Alexa for Business metadata associated with an Amazon Chime user,
-- used to integrate Alexa for Business with a device.
--
-- /See:/ 'newAlexaForBusinessMetadata' smart constructor.
data AlexaForBusinessMetadata = AlexaForBusinessMetadata'
  { -- | The ARN of the room resource.
    alexaForBusinessRoomArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Starts or stops Alexa for Business.
    isAlexaForBusinessEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlexaForBusinessMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alexaForBusinessRoomArn', 'alexaForBusinessMetadata_alexaForBusinessRoomArn' - The ARN of the room resource.
--
-- 'isAlexaForBusinessEnabled', 'alexaForBusinessMetadata_isAlexaForBusinessEnabled' - Starts or stops Alexa for Business.
newAlexaForBusinessMetadata ::
  AlexaForBusinessMetadata
newAlexaForBusinessMetadata =
  AlexaForBusinessMetadata'
    { alexaForBusinessRoomArn =
        Prelude.Nothing,
      isAlexaForBusinessEnabled = Prelude.Nothing
    }

-- | The ARN of the room resource.
alexaForBusinessMetadata_alexaForBusinessRoomArn :: Lens.Lens' AlexaForBusinessMetadata (Prelude.Maybe Prelude.Text)
alexaForBusinessMetadata_alexaForBusinessRoomArn = Lens.lens (\AlexaForBusinessMetadata' {alexaForBusinessRoomArn} -> alexaForBusinessRoomArn) (\s@AlexaForBusinessMetadata' {} a -> s {alexaForBusinessRoomArn = a} :: AlexaForBusinessMetadata) Prelude.. Lens.mapping Data._Sensitive

-- | Starts or stops Alexa for Business.
alexaForBusinessMetadata_isAlexaForBusinessEnabled :: Lens.Lens' AlexaForBusinessMetadata (Prelude.Maybe Prelude.Bool)
alexaForBusinessMetadata_isAlexaForBusinessEnabled = Lens.lens (\AlexaForBusinessMetadata' {isAlexaForBusinessEnabled} -> isAlexaForBusinessEnabled) (\s@AlexaForBusinessMetadata' {} a -> s {isAlexaForBusinessEnabled = a} :: AlexaForBusinessMetadata)

instance Data.FromJSON AlexaForBusinessMetadata where
  parseJSON =
    Data.withObject
      "AlexaForBusinessMetadata"
      ( \x ->
          AlexaForBusinessMetadata'
            Prelude.<$> (x Data..:? "AlexaForBusinessRoomArn")
            Prelude.<*> (x Data..:? "IsAlexaForBusinessEnabled")
      )

instance Prelude.Hashable AlexaForBusinessMetadata where
  hashWithSalt _salt AlexaForBusinessMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` alexaForBusinessRoomArn
      `Prelude.hashWithSalt` isAlexaForBusinessEnabled

instance Prelude.NFData AlexaForBusinessMetadata where
  rnf AlexaForBusinessMetadata' {..} =
    Prelude.rnf alexaForBusinessRoomArn
      `Prelude.seq` Prelude.rnf isAlexaForBusinessEnabled

instance Data.ToJSON AlexaForBusinessMetadata where
  toJSON AlexaForBusinessMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlexaForBusinessRoomArn" Data..=)
              Prelude.<$> alexaForBusinessRoomArn,
            ("IsAlexaForBusinessEnabled" Data..=)
              Prelude.<$> isAlexaForBusinessEnabled
          ]
      )
