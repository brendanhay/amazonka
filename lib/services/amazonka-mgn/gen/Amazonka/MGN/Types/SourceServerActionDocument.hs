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
-- Module      : Amazonka.MGN.Types.SourceServerActionDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SourceServerActionDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.SsmParameterStoreParameter
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newSourceServerActionDocument' smart constructor.
data SourceServerActionDocument = SourceServerActionDocument'
  { -- | Source server post migration custom action ID.
    actionID :: Prelude.Maybe Prelude.Text,
    -- | Source server post migration custom action name.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | Source server post migration custom action active status.
    active :: Prelude.Maybe Prelude.Bool,
    -- | Source server post migration custom action document identifier.
    documentIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Source server post migration custom action document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Source server post migration custom action must succeed for cutover.
    mustSucceedForCutover :: Prelude.Maybe Prelude.Bool,
    -- | Source server post migration custom action order.
    order :: Prelude.Maybe Prelude.Natural,
    -- | Source server post migration custom action parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]),
    -- | Source server post migration custom action timeout in seconds.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceServerActionDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionID', 'sourceServerActionDocument_actionID' - Source server post migration custom action ID.
--
-- 'actionName', 'sourceServerActionDocument_actionName' - Source server post migration custom action name.
--
-- 'active', 'sourceServerActionDocument_active' - Source server post migration custom action active status.
--
-- 'documentIdentifier', 'sourceServerActionDocument_documentIdentifier' - Source server post migration custom action document identifier.
--
-- 'documentVersion', 'sourceServerActionDocument_documentVersion' - Source server post migration custom action document version.
--
-- 'mustSucceedForCutover', 'sourceServerActionDocument_mustSucceedForCutover' - Source server post migration custom action must succeed for cutover.
--
-- 'order', 'sourceServerActionDocument_order' - Source server post migration custom action order.
--
-- 'parameters', 'sourceServerActionDocument_parameters' - Source server post migration custom action parameters.
--
-- 'timeoutSeconds', 'sourceServerActionDocument_timeoutSeconds' - Source server post migration custom action timeout in seconds.
newSourceServerActionDocument ::
  SourceServerActionDocument
newSourceServerActionDocument =
  SourceServerActionDocument'
    { actionID =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      active = Prelude.Nothing,
      documentIdentifier = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      mustSucceedForCutover = Prelude.Nothing,
      order = Prelude.Nothing,
      parameters = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing
    }

-- | Source server post migration custom action ID.
sourceServerActionDocument_actionID :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Text)
sourceServerActionDocument_actionID = Lens.lens (\SourceServerActionDocument' {actionID} -> actionID) (\s@SourceServerActionDocument' {} a -> s {actionID = a} :: SourceServerActionDocument)

-- | Source server post migration custom action name.
sourceServerActionDocument_actionName :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Text)
sourceServerActionDocument_actionName = Lens.lens (\SourceServerActionDocument' {actionName} -> actionName) (\s@SourceServerActionDocument' {} a -> s {actionName = a} :: SourceServerActionDocument)

-- | Source server post migration custom action active status.
sourceServerActionDocument_active :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Bool)
sourceServerActionDocument_active = Lens.lens (\SourceServerActionDocument' {active} -> active) (\s@SourceServerActionDocument' {} a -> s {active = a} :: SourceServerActionDocument)

-- | Source server post migration custom action document identifier.
sourceServerActionDocument_documentIdentifier :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Text)
sourceServerActionDocument_documentIdentifier = Lens.lens (\SourceServerActionDocument' {documentIdentifier} -> documentIdentifier) (\s@SourceServerActionDocument' {} a -> s {documentIdentifier = a} :: SourceServerActionDocument)

-- | Source server post migration custom action document version.
sourceServerActionDocument_documentVersion :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Text)
sourceServerActionDocument_documentVersion = Lens.lens (\SourceServerActionDocument' {documentVersion} -> documentVersion) (\s@SourceServerActionDocument' {} a -> s {documentVersion = a} :: SourceServerActionDocument)

-- | Source server post migration custom action must succeed for cutover.
sourceServerActionDocument_mustSucceedForCutover :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Bool)
sourceServerActionDocument_mustSucceedForCutover = Lens.lens (\SourceServerActionDocument' {mustSucceedForCutover} -> mustSucceedForCutover) (\s@SourceServerActionDocument' {} a -> s {mustSucceedForCutover = a} :: SourceServerActionDocument)

-- | Source server post migration custom action order.
sourceServerActionDocument_order :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Natural)
sourceServerActionDocument_order = Lens.lens (\SourceServerActionDocument' {order} -> order) (\s@SourceServerActionDocument' {} a -> s {order = a} :: SourceServerActionDocument)

-- | Source server post migration custom action parameters.
sourceServerActionDocument_parameters :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]))
sourceServerActionDocument_parameters = Lens.lens (\SourceServerActionDocument' {parameters} -> parameters) (\s@SourceServerActionDocument' {} a -> s {parameters = a} :: SourceServerActionDocument) Prelude.. Lens.mapping Lens.coerced

-- | Source server post migration custom action timeout in seconds.
sourceServerActionDocument_timeoutSeconds :: Lens.Lens' SourceServerActionDocument (Prelude.Maybe Prelude.Natural)
sourceServerActionDocument_timeoutSeconds = Lens.lens (\SourceServerActionDocument' {timeoutSeconds} -> timeoutSeconds) (\s@SourceServerActionDocument' {} a -> s {timeoutSeconds = a} :: SourceServerActionDocument)

instance Data.FromJSON SourceServerActionDocument where
  parseJSON =
    Data.withObject
      "SourceServerActionDocument"
      ( \x ->
          SourceServerActionDocument'
            Prelude.<$> (x Data..:? "actionID")
            Prelude.<*> (x Data..:? "actionName")
            Prelude.<*> (x Data..:? "active")
            Prelude.<*> (x Data..:? "documentIdentifier")
            Prelude.<*> (x Data..:? "documentVersion")
            Prelude.<*> (x Data..:? "mustSucceedForCutover")
            Prelude.<*> (x Data..:? "order")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timeoutSeconds")
      )

instance Prelude.Hashable SourceServerActionDocument where
  hashWithSalt _salt SourceServerActionDocument' {..} =
    _salt
      `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` documentIdentifier
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` mustSucceedForCutover
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` timeoutSeconds

instance Prelude.NFData SourceServerActionDocument where
  rnf SourceServerActionDocument' {..} =
    Prelude.rnf actionID `Prelude.seq`
      Prelude.rnf actionName `Prelude.seq`
        Prelude.rnf active `Prelude.seq`
          Prelude.rnf documentIdentifier `Prelude.seq`
            Prelude.rnf documentVersion `Prelude.seq`
              Prelude.rnf mustSucceedForCutover `Prelude.seq`
                Prelude.rnf order `Prelude.seq`
                  Prelude.rnf parameters `Prelude.seq`
                    Prelude.rnf timeoutSeconds
