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
-- Module      : Amazonka.MGN.PutSourceServerAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Put source server post migration custom action.
module Amazonka.MGN.PutSourceServerAction
  ( -- * Creating a Request
    PutSourceServerAction (..),
    newPutSourceServerAction,

    -- * Request Lenses
    putSourceServerAction_active,
    putSourceServerAction_documentVersion,
    putSourceServerAction_mustSucceedForCutover,
    putSourceServerAction_parameters,
    putSourceServerAction_timeoutSeconds,
    putSourceServerAction_actionID,
    putSourceServerAction_actionName,
    putSourceServerAction_documentIdentifier,
    putSourceServerAction_order,
    putSourceServerAction_sourceServerID,

    -- * Destructuring the Response
    SourceServerActionDocument (..),
    newSourceServerActionDocument,

    -- * Response Lenses
    sourceServerActionDocument_actionID,
    sourceServerActionDocument_actionName,
    sourceServerActionDocument_active,
    sourceServerActionDocument_documentIdentifier,
    sourceServerActionDocument_documentVersion,
    sourceServerActionDocument_mustSucceedForCutover,
    sourceServerActionDocument_order,
    sourceServerActionDocument_parameters,
    sourceServerActionDocument_timeoutSeconds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSourceServerAction' smart constructor.
data PutSourceServerAction = PutSourceServerAction'
  { -- | Source server post migration custom action active status.
    active :: Prelude.Maybe Prelude.Bool,
    -- | Source server post migration custom action document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Source server post migration custom action must succeed for cutover.
    mustSucceedForCutover :: Prelude.Maybe Prelude.Bool,
    -- | Source server post migration custom action parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]),
    -- | Source server post migration custom action timeout in seconds.
    timeoutSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Source server post migration custom action ID.
    actionID :: Prelude.Text,
    -- | Source server post migration custom action name.
    actionName :: Prelude.Text,
    -- | Source server post migration custom action document identifier.
    documentIdentifier :: Prelude.Text,
    -- | Source server post migration custom action order.
    order :: Prelude.Natural,
    -- | Source server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSourceServerAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'putSourceServerAction_active' - Source server post migration custom action active status.
--
-- 'documentVersion', 'putSourceServerAction_documentVersion' - Source server post migration custom action document version.
--
-- 'mustSucceedForCutover', 'putSourceServerAction_mustSucceedForCutover' - Source server post migration custom action must succeed for cutover.
--
-- 'parameters', 'putSourceServerAction_parameters' - Source server post migration custom action parameters.
--
-- 'timeoutSeconds', 'putSourceServerAction_timeoutSeconds' - Source server post migration custom action timeout in seconds.
--
-- 'actionID', 'putSourceServerAction_actionID' - Source server post migration custom action ID.
--
-- 'actionName', 'putSourceServerAction_actionName' - Source server post migration custom action name.
--
-- 'documentIdentifier', 'putSourceServerAction_documentIdentifier' - Source server post migration custom action document identifier.
--
-- 'order', 'putSourceServerAction_order' - Source server post migration custom action order.
--
-- 'sourceServerID', 'putSourceServerAction_sourceServerID' - Source server ID.
newPutSourceServerAction ::
  -- | 'actionID'
  Prelude.Text ->
  -- | 'actionName'
  Prelude.Text ->
  -- | 'documentIdentifier'
  Prelude.Text ->
  -- | 'order'
  Prelude.Natural ->
  -- | 'sourceServerID'
  Prelude.Text ->
  PutSourceServerAction
newPutSourceServerAction
  pActionID_
  pActionName_
  pDocumentIdentifier_
  pOrder_
  pSourceServerID_ =
    PutSourceServerAction'
      { active = Prelude.Nothing,
        documentVersion = Prelude.Nothing,
        mustSucceedForCutover = Prelude.Nothing,
        parameters = Prelude.Nothing,
        timeoutSeconds = Prelude.Nothing,
        actionID = pActionID_,
        actionName = pActionName_,
        documentIdentifier = pDocumentIdentifier_,
        order = pOrder_,
        sourceServerID = pSourceServerID_
      }

-- | Source server post migration custom action active status.
putSourceServerAction_active :: Lens.Lens' PutSourceServerAction (Prelude.Maybe Prelude.Bool)
putSourceServerAction_active = Lens.lens (\PutSourceServerAction' {active} -> active) (\s@PutSourceServerAction' {} a -> s {active = a} :: PutSourceServerAction)

-- | Source server post migration custom action document version.
putSourceServerAction_documentVersion :: Lens.Lens' PutSourceServerAction (Prelude.Maybe Prelude.Text)
putSourceServerAction_documentVersion = Lens.lens (\PutSourceServerAction' {documentVersion} -> documentVersion) (\s@PutSourceServerAction' {} a -> s {documentVersion = a} :: PutSourceServerAction)

-- | Source server post migration custom action must succeed for cutover.
putSourceServerAction_mustSucceedForCutover :: Lens.Lens' PutSourceServerAction (Prelude.Maybe Prelude.Bool)
putSourceServerAction_mustSucceedForCutover = Lens.lens (\PutSourceServerAction' {mustSucceedForCutover} -> mustSucceedForCutover) (\s@PutSourceServerAction' {} a -> s {mustSucceedForCutover = a} :: PutSourceServerAction)

-- | Source server post migration custom action parameters.
putSourceServerAction_parameters :: Lens.Lens' PutSourceServerAction (Prelude.Maybe (Prelude.HashMap Prelude.Text [SsmParameterStoreParameter]))
putSourceServerAction_parameters = Lens.lens (\PutSourceServerAction' {parameters} -> parameters) (\s@PutSourceServerAction' {} a -> s {parameters = a} :: PutSourceServerAction) Prelude.. Lens.mapping Lens.coerced

-- | Source server post migration custom action timeout in seconds.
putSourceServerAction_timeoutSeconds :: Lens.Lens' PutSourceServerAction (Prelude.Maybe Prelude.Natural)
putSourceServerAction_timeoutSeconds = Lens.lens (\PutSourceServerAction' {timeoutSeconds} -> timeoutSeconds) (\s@PutSourceServerAction' {} a -> s {timeoutSeconds = a} :: PutSourceServerAction)

-- | Source server post migration custom action ID.
putSourceServerAction_actionID :: Lens.Lens' PutSourceServerAction Prelude.Text
putSourceServerAction_actionID = Lens.lens (\PutSourceServerAction' {actionID} -> actionID) (\s@PutSourceServerAction' {} a -> s {actionID = a} :: PutSourceServerAction)

-- | Source server post migration custom action name.
putSourceServerAction_actionName :: Lens.Lens' PutSourceServerAction Prelude.Text
putSourceServerAction_actionName = Lens.lens (\PutSourceServerAction' {actionName} -> actionName) (\s@PutSourceServerAction' {} a -> s {actionName = a} :: PutSourceServerAction)

-- | Source server post migration custom action document identifier.
putSourceServerAction_documentIdentifier :: Lens.Lens' PutSourceServerAction Prelude.Text
putSourceServerAction_documentIdentifier = Lens.lens (\PutSourceServerAction' {documentIdentifier} -> documentIdentifier) (\s@PutSourceServerAction' {} a -> s {documentIdentifier = a} :: PutSourceServerAction)

-- | Source server post migration custom action order.
putSourceServerAction_order :: Lens.Lens' PutSourceServerAction Prelude.Natural
putSourceServerAction_order = Lens.lens (\PutSourceServerAction' {order} -> order) (\s@PutSourceServerAction' {} a -> s {order = a} :: PutSourceServerAction)

-- | Source server ID.
putSourceServerAction_sourceServerID :: Lens.Lens' PutSourceServerAction Prelude.Text
putSourceServerAction_sourceServerID = Lens.lens (\PutSourceServerAction' {sourceServerID} -> sourceServerID) (\s@PutSourceServerAction' {} a -> s {sourceServerID = a} :: PutSourceServerAction)

instance Core.AWSRequest PutSourceServerAction where
  type
    AWSResponse PutSourceServerAction =
      SourceServerActionDocument
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutSourceServerAction where
  hashWithSalt _salt PutSourceServerAction' {..} =
    _salt
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` mustSucceedForCutover
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` timeoutSeconds
      `Prelude.hashWithSalt` actionID
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` documentIdentifier
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData PutSourceServerAction where
  rnf PutSourceServerAction' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf mustSucceedForCutover
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf actionID
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf documentIdentifier
      `Prelude.seq` Prelude.rnf order
      `Prelude.seq` Prelude.rnf sourceServerID

instance Data.ToHeaders PutSourceServerAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutSourceServerAction where
  toJSON PutSourceServerAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("active" Data..=) Prelude.<$> active,
            ("documentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("mustSucceedForCutover" Data..=)
              Prelude.<$> mustSucceedForCutover,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("timeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds,
            Prelude.Just ("actionID" Data..= actionID),
            Prelude.Just ("actionName" Data..= actionName),
            Prelude.Just
              ("documentIdentifier" Data..= documentIdentifier),
            Prelude.Just ("order" Data..= order),
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath PutSourceServerAction where
  toPath = Prelude.const "/PutSourceServerAction"

instance Data.ToQuery PutSourceServerAction where
  toQuery = Prelude.const Prelude.mempty
