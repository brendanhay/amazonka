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
-- Module      : Amazonka.SageMakerEdge.Types.Definition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.Definition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.Checksum
import Amazonka.SageMakerEdge.Types.ModelState

-- |
--
-- /See:/ 'newDefinition' smart constructor.
data Definition = Definition'
  { -- | The desired state of the model.
    state :: Prelude.Maybe ModelState,
    -- | The checksum information of the model.
    checksum :: Prelude.Maybe Checksum,
    -- | The absolute S3 location of the model.
    s3Url :: Prelude.Maybe Prelude.Text,
    -- | The unique model handle.
    modelHandle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Definition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'definition_state' - The desired state of the model.
--
-- 'checksum', 'definition_checksum' - The checksum information of the model.
--
-- 's3Url', 'definition_s3Url' - The absolute S3 location of the model.
--
-- 'modelHandle', 'definition_modelHandle' - The unique model handle.
newDefinition ::
  Definition
newDefinition =
  Definition'
    { state = Prelude.Nothing,
      checksum = Prelude.Nothing,
      s3Url = Prelude.Nothing,
      modelHandle = Prelude.Nothing
    }

-- | The desired state of the model.
definition_state :: Lens.Lens' Definition (Prelude.Maybe ModelState)
definition_state = Lens.lens (\Definition' {state} -> state) (\s@Definition' {} a -> s {state = a} :: Definition)

-- | The checksum information of the model.
definition_checksum :: Lens.Lens' Definition (Prelude.Maybe Checksum)
definition_checksum = Lens.lens (\Definition' {checksum} -> checksum) (\s@Definition' {} a -> s {checksum = a} :: Definition)

-- | The absolute S3 location of the model.
definition_s3Url :: Lens.Lens' Definition (Prelude.Maybe Prelude.Text)
definition_s3Url = Lens.lens (\Definition' {s3Url} -> s3Url) (\s@Definition' {} a -> s {s3Url = a} :: Definition)

-- | The unique model handle.
definition_modelHandle :: Lens.Lens' Definition (Prelude.Maybe Prelude.Text)
definition_modelHandle = Lens.lens (\Definition' {modelHandle} -> modelHandle) (\s@Definition' {} a -> s {modelHandle = a} :: Definition)

instance Data.FromJSON Definition where
  parseJSON =
    Data.withObject
      "Definition"
      ( \x ->
          Definition'
            Prelude.<$> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Checksum")
            Prelude.<*> (x Data..:? "S3Url")
            Prelude.<*> (x Data..:? "ModelHandle")
      )

instance Prelude.Hashable Definition where
  hashWithSalt _salt Definition' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` s3Url
      `Prelude.hashWithSalt` modelHandle

instance Prelude.NFData Definition where
  rnf Definition' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf s3Url
      `Prelude.seq` Prelude.rnf modelHandle
