{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.PutTemplateAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Put template post migration custom action.
module Amazonka.MGN.PutTemplateAction
  ( -- * Creating a Request
    PutTemplateAction (..),
    newPutTemplateAction,

    -- * Request Lenses
    putTemplateAction_active,
    putTemplateAction_documentVersion,
    putTemplateAction_mustSucceedForCutover,
    putTemplateAction_operatingSystem,
    putTemplateAction_parameters,
    putTemplateAction_timeoutSeconds,
    putTemplateAction_actionID,
    putTemplateAction_actionName,
    putTemplateAction_documentIdentifier,
    putTemplateAction_launchConfigurationTemplateID,
    putTemplateAction_order,

    -- * Destructuring the Response
    TemplateActionDocument (..),
    newTemplateActionDocument,

    -- * Response Lenses
    templateActionDocument_actionID,
    templateActionDocument_actionName,
    templateActionDocument_active,
    templateActionDocument_documentIdentifier,
    templateActionDocument_documentVersion,
    templateActionDocument_mustSucceedForCutover,
    templateActionDocument_operatingSystem,
    templateActionDocument_order,
    templateActionDocument_parameters,
    templateActionDocument_timeoutSeconds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutTemplateAction' smart constructor.
data PutTemplateAction = PutTemplateAction'
  { -- | Template post migration custom action active status.
    active :: Prelude.Maybe Prelude.Bool,
    -- | Template post migration custom action document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action must succeed for cutover.
    mustSucceedForCutover :: Prelude.Maybe Prelude.Bool,
    -- | Operating system eligible for this template post migration custom
    -- action.
    operatingSystem :: Prelude.Maybe Prelude.Text,
    -- | Template post migration custom action parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]),
    -- | Template post migration custom action timeout in seconds.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Template post migration custom action ID.
    actionID :: Prelude.Text,
    -- | Template post migration custom action name.
    actionName :: Prelude.Text,
    -- | Template post migration custom action document identifier.
    documentIdentifier :: Prelude.Text,
    -- | Launch configuration template ID.
    launchConfigurationTemplateID :: Prelude.Text,
    -- | Template post migration custom action order.
    order :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutTemplateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'putTemplateAction_active' - Template post migration custom action active status.
--
-- 'documentVersion', 'putTemplateAction_documentVersion' - Template post migration custom action document version.
--
-- 'mustSucceedForCutover', 'putTemplateAction_mustSucceedForCutover' - Template post migration custom action must succeed for cutover.
--
-- 'operatingSystem', 'putTemplateAction_operatingSystem' - Operating system eligible for this template post migration custom
-- action.
--
-- 'parameters', 'putTemplateAction_parameters' - Template post migration custom action parameters.
--
-- 'timeoutSeconds', 'putTemplateAction_timeoutSeconds' - Template post migration custom action timeout in seconds.
--
-- 'actionID', 'putTemplateAction_actionID' - Template post migration custom action ID.
--
-- 'actionName', 'putTemplateAction_actionName' - Template post migration custom action name.
--
-- 'documentIdentifier', 'putTemplateAction_documentIdentifier' - Template post migration custom action document identifier.
--
-- 'launchConfigurationTemplateID', 'putTemplateAction_launchConfigurationTemplateID' - Launch configuration template ID.
--
-- 'order', 'putTemplateAction_order' - Template post migration custom action order.
newPutTemplateAction ::
  -- | 'actionID'
  Prelude.Text ->
  -- | 'actionName'
  Prelude.Text ->
  -- | 'documentIdentifier'
  Prelude.Text ->
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  -- | 'order'
  Prelude.Natural ->
  PutTemplateAction
newPutTemplateAction
  pActionID_
  pActionName_
  pDocumentIdentifier_
  pLaunchConfigurationTemplateID_
  pOrder_ =
    PutTemplateAction'
      { active = Prelude.Nothing,
        documentVersion = Prelude.Nothing,
        mustSucceedForCutover = Prelude.Nothing,
        operatingSystem = Prelude.Nothing,
        parameters = Prelude.Nothing,
        timeoutSeconds = Prelude.Nothing,
        actionID = pActionID_,
        actionName = pActionName_,
        documentIdentifier = pDocumentIdentifier_,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_,
        order = pOrder_
      }

-- | Template post migration custom action active status.
putTemplateAction_active :: Lens.Lens' PutTemplateAction (Prelude.Maybe Prelude.Bool)
putTemplateAction_active = Lens.lens (\PutTemplateAction' {active} -> active) (\s@PutTemplateAction' {} a -> s {active = a} :: PutTemplateAction)

-- | Template post migration custom action document version.
putTemplateAction_documentVersion :: Lens.Lens' PutTemplateAction (Prelude.Maybe Prelude.Text)
putTemplateAction_documentVersion = Lens.lens (\PutTemplateAction' {documentVersion} -> documentVersion) (\s@PutTemplateAction' {} a -> s {documentVersion = a} :: PutTemplateAction)

-- | Template post migration custom action must succeed for cutover.
putTemplateAction_mustSucceedForCutover :: Lens.Lens' PutTemplateAction (Prelude.Maybe Prelude.Bool)
putTemplateAction_mustSucceedForCutover = Lens.lens (\PutTemplateAction' {mustSucceedForCutover} -> mustSucceedForCutover) (\s@PutTemplateAction' {} a -> s {mustSucceedForCutover = a} :: PutTemplateAction)

-- | Operating system eligible for this template post migration custom
-- action.
putTemplateAction_operatingSystem :: Lens.Lens' PutTemplateAction (Prelude.Maybe Prelude.Text)
putTemplateAction_operatingSystem = Lens.lens (\PutTemplateAction' {operatingSystem} -> operatingSystem) (\s@PutTemplateAction' {} a -> s {operatingSystem = a} :: PutTemplateAction)

-- | Template post migration custom action parameters.
putTemplateAction_parameters :: Lens.Lens' PutTemplateAction (Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]))
putTemplateAction_parameters = Lens.lens (\PutTemplateAction' {parameters} -> parameters) (\s@PutTemplateAction' {} a -> s {parameters = a} :: PutTemplateAction) Prelude.. Lens.mapping Lens.coerced

-- | Template post migration custom action timeout in seconds.
putTemplateAction_timeoutSeconds :: Lens.Lens' PutTemplateAction (Prelude.Maybe Prelude.Natural)
putTemplateAction_timeoutSeconds = Lens.lens (\PutTemplateAction' {timeoutSeconds} -> timeoutSeconds) (\s@PutTemplateAction' {} a -> s {timeoutSeconds = a} :: PutTemplateAction)

-- | Template post migration custom action ID.
putTemplateAction_actionID :: Lens.Lens' PutTemplateAction Prelude.Text
putTemplateAction_actionID = Lens.lens (\PutTemplateAction' {actionID} -> actionID) (\s@PutTemplateAction' {} a -> s {actionID = a} :: PutTemplateAction)

-- | Template post migration custom action name.
putTemplateAction_actionName :: Lens.Lens' PutTemplateAction Prelude.Text
putTemplateAction_actionName = Lens.lens (\PutTemplateAction' {actionName} -> actionName) (\s@PutTemplateAction' {} a -> s {actionName = a} :: PutTemplateAction)

-- | Template post migration custom action document identifier.
putTemplateAction_documentIdentifier :: Lens.Lens' PutTemplateAction Prelude.Text
putTemplateAction_documentIdentifier = Lens.lens (\PutTemplateAction' {documentIdentifier} -> documentIdentifier) (\s@PutTemplateAction' {} a -> s {documentIdentifier = a} :: PutTemplateAction)

-- | Launch configuration template ID.
putTemplateAction_launchConfigurationTemplateID :: Lens.Lens' PutTemplateAction Prelude.Text
putTemplateAction_launchConfigurationTemplateID = Lens.lens (\PutTemplateAction' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@PutTemplateAction' {} a -> s {launchConfigurationTemplateID = a} :: PutTemplateAction)

-- | Template post migration custom action order.
putTemplateAction_order :: Lens.Lens' PutTemplateAction Prelude.Natural
putTemplateAction_order = Lens.lens (\PutTemplateAction' {order} -> order) (\s@PutTemplateAction' {} a -> s {order = a} :: PutTemplateAction)

instance Core.AWSRequest PutTemplateAction where
  type
    AWSResponse PutTemplateAction =
      TemplateActionDocument
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutTemplateAction where
  hashWithSalt _salt PutTemplateAction' {..} =
    _salt
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` mustSucceedForCutover
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` documentIdentifier
      `Prelude.hashWithSalt` launchConfigurationTemplateID
      `Prelude.hashWithSalt` order

instance Prelude.NFData PutTemplateAction where
  rnf PutTemplateAction' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf mustSucceedForCutover
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf actionID
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf documentIdentifier
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID
      `Prelude.seq` Prelude.rnf order

instance Data.ToHeaders PutTemplateAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutTemplateAction where
  toJSON PutTemplateAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("active" Data..=) Prelude.<$> active,
            ("documentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("mustSucceedForCutover" Data..=)
              Prelude.<$> mustSucceedForCutover,
            ("operatingSystem" Data..=)
              Prelude.<$> operatingSystem,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("timeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds,
            Prelude.Just ("actionID" Data..= actionID),
            Prelude.Just ("actionName" Data..= actionName),
            Prelude.Just
              ("documentIdentifier" Data..= documentIdentifier),
            Prelude.Just
              ( "launchConfigurationTemplateID"
                  Data..= launchConfigurationTemplateID
              ),
            Prelude.Just ("order" Data..= order)
          ]
      )

instance Data.ToPath PutTemplateAction where
  toPath = Prelude.const "/PutTemplateAction"

instance Data.ToQuery PutTemplateAction where
  toQuery = Prelude.const Prelude.mempty
