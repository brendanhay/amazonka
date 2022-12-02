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
-- Module      : Amazonka.MGN.Types.SsmDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SsmDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.SsmParameterStoreParameter
import qualified Amazonka.Prelude as Prelude

-- | Source server replication type.
--
-- /See:/ 'newSsmDocument' smart constructor.
data SsmDocument = SsmDocument'
  { -- | Source server replication type.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Source server replication type.
    mustSucceedForCutover :: Prelude.Maybe Prelude.Bool,
    -- | Source server replication type.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]),
    -- | Source server replication type.
    actionName :: Prelude.Text,
    -- | Source server replication type.
    ssmDocumentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SsmDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutSeconds', 'ssmDocument_timeoutSeconds' - Source server replication type.
--
-- 'mustSucceedForCutover', 'ssmDocument_mustSucceedForCutover' - Source server replication type.
--
-- 'parameters', 'ssmDocument_parameters' - Source server replication type.
--
-- 'actionName', 'ssmDocument_actionName' - Source server replication type.
--
-- 'ssmDocumentName', 'ssmDocument_ssmDocumentName' - Source server replication type.
newSsmDocument ::
  -- | 'actionName'
  Prelude.Text ->
  -- | 'ssmDocumentName'
  Prelude.Text ->
  SsmDocument
newSsmDocument pActionName_ pSsmDocumentName_ =
  SsmDocument'
    { timeoutSeconds = Prelude.Nothing,
      mustSucceedForCutover = Prelude.Nothing,
      parameters = Prelude.Nothing,
      actionName = pActionName_,
      ssmDocumentName = pSsmDocumentName_
    }

-- | Source server replication type.
ssmDocument_timeoutSeconds :: Lens.Lens' SsmDocument (Prelude.Maybe Prelude.Natural)
ssmDocument_timeoutSeconds = Lens.lens (\SsmDocument' {timeoutSeconds} -> timeoutSeconds) (\s@SsmDocument' {} a -> s {timeoutSeconds = a} :: SsmDocument)

-- | Source server replication type.
ssmDocument_mustSucceedForCutover :: Lens.Lens' SsmDocument (Prelude.Maybe Prelude.Bool)
ssmDocument_mustSucceedForCutover = Lens.lens (\SsmDocument' {mustSucceedForCutover} -> mustSucceedForCutover) (\s@SsmDocument' {} a -> s {mustSucceedForCutover = a} :: SsmDocument)

-- | Source server replication type.
ssmDocument_parameters :: Lens.Lens' SsmDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]))
ssmDocument_parameters = Lens.lens (\SsmDocument' {parameters} -> parameters) (\s@SsmDocument' {} a -> s {parameters = a} :: SsmDocument) Prelude.. Lens.mapping Lens.coerced

-- | Source server replication type.
ssmDocument_actionName :: Lens.Lens' SsmDocument Prelude.Text
ssmDocument_actionName = Lens.lens (\SsmDocument' {actionName} -> actionName) (\s@SsmDocument' {} a -> s {actionName = a} :: SsmDocument)

-- | Source server replication type.
ssmDocument_ssmDocumentName :: Lens.Lens' SsmDocument Prelude.Text
ssmDocument_ssmDocumentName = Lens.lens (\SsmDocument' {ssmDocumentName} -> ssmDocumentName) (\s@SsmDocument' {} a -> s {ssmDocumentName = a} :: SsmDocument)

instance Data.FromJSON SsmDocument where
  parseJSON =
    Data.withObject
      "SsmDocument"
      ( \x ->
          SsmDocument'
            Prelude.<$> (x Data..:? "timeoutSeconds")
            Prelude.<*> (x Data..:? "mustSucceedForCutover")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "actionName")
            Prelude.<*> (x Data..: "ssmDocumentName")
      )

instance Prelude.Hashable SsmDocument where
  hashWithSalt _salt SsmDocument' {..} =
    _salt `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` mustSucceedForCutover
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` ssmDocumentName

instance Prelude.NFData SsmDocument where
  rnf SsmDocument' {..} =
    Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf mustSucceedForCutover
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf ssmDocumentName

instance Data.ToJSON SsmDocument where
  toJSON SsmDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds,
            ("mustSucceedForCutover" Data..=)
              Prelude.<$> mustSucceedForCutover,
            ("parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("actionName" Data..= actionName),
            Prelude.Just
              ("ssmDocumentName" Data..= ssmDocumentName)
          ]
      )
