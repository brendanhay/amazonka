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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SsmDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.SsmParameterStoreParameter
import qualified Amazonka.Prelude as Prelude

-- | AWS Systems Manager Document.
--
-- /See:/ 'newSsmDocument' smart constructor.
data SsmDocument = SsmDocument'
  { -- | If true, Cutover will not be enabled if the document has failed.
    mustSucceedForCutover :: Prelude.Maybe Prelude.Bool,
    -- | AWS Systems Manager Document parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]),
    -- | AWS Systems Manager Document timeout seconds.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | User-friendly name for the AWS Systems Manager Document.
    actionName :: Prelude.Text,
    -- | AWS Systems Manager Document name or full ARN.
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
-- 'mustSucceedForCutover', 'ssmDocument_mustSucceedForCutover' - If true, Cutover will not be enabled if the document has failed.
--
-- 'parameters', 'ssmDocument_parameters' - AWS Systems Manager Document parameters.
--
-- 'timeoutSeconds', 'ssmDocument_timeoutSeconds' - AWS Systems Manager Document timeout seconds.
--
-- 'actionName', 'ssmDocument_actionName' - User-friendly name for the AWS Systems Manager Document.
--
-- 'ssmDocumentName', 'ssmDocument_ssmDocumentName' - AWS Systems Manager Document name or full ARN.
newSsmDocument ::
  -- | 'actionName'
  Prelude.Text ->
  -- | 'ssmDocumentName'
  Prelude.Text ->
  SsmDocument
newSsmDocument pActionName_ pSsmDocumentName_ =
  SsmDocument'
    { mustSucceedForCutover =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      actionName = pActionName_,
      ssmDocumentName = pSsmDocumentName_
    }

-- | If true, Cutover will not be enabled if the document has failed.
ssmDocument_mustSucceedForCutover :: Lens.Lens' SsmDocument (Prelude.Maybe Prelude.Bool)
ssmDocument_mustSucceedForCutover = Lens.lens (\SsmDocument' {mustSucceedForCutover} -> mustSucceedForCutover) (\s@SsmDocument' {} a -> s {mustSucceedForCutover = a} :: SsmDocument)

-- | AWS Systems Manager Document parameters.
ssmDocument_parameters :: Lens.Lens' SsmDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]))
ssmDocument_parameters = Lens.lens (\SsmDocument' {parameters} -> parameters) (\s@SsmDocument' {} a -> s {parameters = a} :: SsmDocument) Prelude.. Lens.mapping Lens.coerced

-- | AWS Systems Manager Document timeout seconds.
ssmDocument_timeoutSeconds :: Lens.Lens' SsmDocument (Prelude.Maybe Prelude.Natural)
ssmDocument_timeoutSeconds = Lens.lens (\SsmDocument' {timeoutSeconds} -> timeoutSeconds) (\s@SsmDocument' {} a -> s {timeoutSeconds = a} :: SsmDocument)

-- | User-friendly name for the AWS Systems Manager Document.
ssmDocument_actionName :: Lens.Lens' SsmDocument Prelude.Text
ssmDocument_actionName = Lens.lens (\SsmDocument' {actionName} -> actionName) (\s@SsmDocument' {} a -> s {actionName = a} :: SsmDocument)

-- | AWS Systems Manager Document name or full ARN.
ssmDocument_ssmDocumentName :: Lens.Lens' SsmDocument Prelude.Text
ssmDocument_ssmDocumentName = Lens.lens (\SsmDocument' {ssmDocumentName} -> ssmDocumentName) (\s@SsmDocument' {} a -> s {ssmDocumentName = a} :: SsmDocument)

instance Data.FromJSON SsmDocument where
  parseJSON =
    Data.withObject
      "SsmDocument"
      ( \x ->
          SsmDocument'
            Prelude.<$> (x Data..:? "mustSucceedForCutover")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timeoutSeconds")
            Prelude.<*> (x Data..: "actionName")
            Prelude.<*> (x Data..: "ssmDocumentName")
      )

instance Prelude.Hashable SsmDocument where
  hashWithSalt _salt SsmDocument' {..} =
    _salt
      `Prelude.hashWithSalt` mustSucceedForCutover
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` ssmDocumentName

instance Prelude.NFData SsmDocument where
  rnf SsmDocument' {..} =
    Prelude.rnf mustSucceedForCutover
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf ssmDocumentName

instance Data.ToJSON SsmDocument where
  toJSON SsmDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mustSucceedForCutover" Data..=)
              Prelude.<$> mustSucceedForCutover,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("timeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds,
            Prelude.Just ("actionName" Data..= actionName),
            Prelude.Just
              ("ssmDocumentName" Data..= ssmDocumentName)
          ]
      )
