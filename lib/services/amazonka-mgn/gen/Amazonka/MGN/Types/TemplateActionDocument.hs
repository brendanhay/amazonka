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
-- Module      : Amazonka.MGN.Types.TemplateActionDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.TemplateActionDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.SsmParameterStoreParameter
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newTemplateActionDocument' smart constructor.
data TemplateActionDocument = TemplateActionDocument'
  { -- | Template post migration custom action ID.
    actionID :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action name.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action active status.
    active :: Prelude.Maybe Prelude.Bool,
    -- | Template post migration custom action document identifier.
    documentIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action must succeed for cutover.
    mustSucceedForCutover :: Prelude.Maybe Prelude.Bool,
    -- | Operating system eligible for this template post migration custom
    -- action.
    operatingSystem :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action order.
    order :: Prelude.Maybe Prelude.Natural,
    -- | Template post migration custom action parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]),
    -- | Template post migration custom action timeout in seconds.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateActionDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionID', 'templateActionDocument_actionID' - Template post migration custom action ID.
--
-- 'actionName', 'templateActionDocument_actionName' - Template post migration custom action name.
--
-- 'active', 'templateActionDocument_active' - Template post migration custom action active status.
--
-- 'documentIdentifier', 'templateActionDocument_documentIdentifier' - Template post migration custom action document identifier.
--
-- 'documentVersion', 'templateActionDocument_documentVersion' - Template post migration custom action document version.
--
-- 'mustSucceedForCutover', 'templateActionDocument_mustSucceedForCutover' - Template post migration custom action must succeed for cutover.
--
-- 'operatingSystem', 'templateActionDocument_operatingSystem' - Operating system eligible for this template post migration custom
-- action.
--
-- 'order', 'templateActionDocument_order' - Template post migration custom action order.
--
-- 'parameters', 'templateActionDocument_parameters' - Template post migration custom action parameters.
--
-- 'timeoutSeconds', 'templateActionDocument_timeoutSeconds' - Template post migration custom action timeout in seconds.
newTemplateActionDocument ::
  TemplateActionDocument
newTemplateActionDocument =
  TemplateActionDocument'
    { actionID = Prelude.Nothing,
      actionName = Prelude.Nothing,
      active = Prelude.Nothing,
      documentIdentifier = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      mustSucceedForCutover = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      order = Prelude.Nothing,
      parameters = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing
    }

-- | Template post migration custom action ID.
templateActionDocument_actionID :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Text)
templateActionDocument_actionID = Lens.lens (\TemplateActionDocument' {actionID} -> actionID) (\s@TemplateActionDocument' {} a -> s {actionID = a} :: TemplateActionDocument)

-- | Template post migration custom action name.
templateActionDocument_actionName :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Text)
templateActionDocument_actionName = Lens.lens (\TemplateActionDocument' {actionName} -> actionName) (\s@TemplateActionDocument' {} a -> s {actionName = a} :: TemplateActionDocument)

-- | Template post migration custom action active status.
templateActionDocument_active :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Bool)
templateActionDocument_active = Lens.lens (\TemplateActionDocument' {active} -> active) (\s@TemplateActionDocument' {} a -> s {active = a} :: TemplateActionDocument)

-- | Template post migration custom action document identifier.
templateActionDocument_documentIdentifier :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Text)
templateActionDocument_documentIdentifier = Lens.lens (\TemplateActionDocument' {documentIdentifier} -> documentIdentifier) (\s@TemplateActionDocument' {} a -> s {documentIdentifier = a} :: TemplateActionDocument)

-- | Template post migration custom action document version.
templateActionDocument_documentVersion :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Text)
templateActionDocument_documentVersion = Lens.lens (\TemplateActionDocument' {documentVersion} -> documentVersion) (\s@TemplateActionDocument' {} a -> s {documentVersion = a} :: TemplateActionDocument)

-- | Template post migration custom action must succeed for cutover.
templateActionDocument_mustSucceedForCutover :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Bool)
templateActionDocument_mustSucceedForCutover = Lens.lens (\TemplateActionDocument' {mustSucceedForCutover} -> mustSucceedForCutover) (\s@TemplateActionDocument' {} a -> s {mustSucceedForCutover = a} :: TemplateActionDocument)

-- | Operating system eligible for this template post migration custom
-- action.
templateActionDocument_operatingSystem :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Text)
templateActionDocument_operatingSystem = Lens.lens (\TemplateActionDocument' {operatingSystem} -> operatingSystem) (\s@TemplateActionDocument' {} a -> s {operatingSystem = a} :: TemplateActionDocument)

-- | Template post migration custom action order.
templateActionDocument_order :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Natural)
templateActionDocument_order = Lens.lens (\TemplateActionDocument' {order} -> order) (\s@TemplateActionDocument' {} a -> s {order = a} :: TemplateActionDocument)

-- | Template post migration custom action parameters.
templateActionDocument_parameters :: Lens.Lens' TemplateActionDocument (Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]))
templateActionDocument_parameters = Lens.lens (\TemplateActionDocument' {parameters} -> parameters) (\s@TemplateActionDocument' {} a -> s {parameters = a} :: TemplateActionDocument) Prelude.. Lens.mapping Lens.coerced

-- | Template post migration custom action timeout in seconds.
templateActionDocument_timeoutSeconds :: Lens.Lens' TemplateActionDocument (Prelude.Maybe Prelude.Natural)
templateActionDocument_timeoutSeconds = Lens.lens (\TemplateActionDocument' {timeoutSeconds} -> timeoutSeconds) (\s@TemplateActionDocument' {} a -> s {timeoutSeconds = a} :: TemplateActionDocument)

instance Data.FromJSON TemplateActionDocument where
  parseJSON =
    Data.withObject
      "TemplateActionDocument"
      ( \x ->
          TemplateActionDocument'
            Prelude.<$> (x Data..:? "actionID")
            Prelude.<*> (x Data..:? "actionName")
            Prelude.<*> (x Data..:? "active")
            Prelude.<*> (x Data..:? "documentIdentifier")
            Prelude.<*> (x Data..:? "documentVersion")
            Prelude.<*> (x Data..:? "mustSucceedForCutover")
            Prelude.<*> (x Data..:? "operatingSystem")
            Prelude.<*> (x Data..:? "order")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timeoutSeconds")
      )

instance Prelude.Hashable TemplateActionDocument where
  hashWithSalt _salt TemplateActionDocument' {..} =
    _salt `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` documentIdentifier
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` mustSucceedForCutover
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` timeoutSeconds

instance Prelude.NFData TemplateActionDocument where
  rnf TemplateActionDocument' {..} =
    Prelude.rnf actionID
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf documentIdentifier
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf mustSucceedForCutover
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf order
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf timeoutSeconds
