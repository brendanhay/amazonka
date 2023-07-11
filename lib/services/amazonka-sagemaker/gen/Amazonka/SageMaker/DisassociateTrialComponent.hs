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
-- Module      : Amazonka.SageMaker.DisassociateTrialComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a trial component from a trial. This doesn\'t effect other
-- trials the component is associated with. Before you can delete a
-- component, you must disassociate the component from all trials it is
-- associated with. To associate a trial component with a trial, call the
-- AssociateTrialComponent API.
--
-- To get a list of the trials a component is associated with, use the
-- Search API. Specify @ExperimentTrialComponent@ for the @Resource@
-- parameter. The list appears in the response under
-- @Results.TrialComponent.Parents@.
module Amazonka.SageMaker.DisassociateTrialComponent
  ( -- * Creating a Request
    DisassociateTrialComponent (..),
    newDisassociateTrialComponent,

    -- * Request Lenses
    disassociateTrialComponent_trialComponentName,
    disassociateTrialComponent_trialName,

    -- * Destructuring the Response
    DisassociateTrialComponentResponse (..),
    newDisassociateTrialComponentResponse,

    -- * Response Lenses
    disassociateTrialComponentResponse_trialArn,
    disassociateTrialComponentResponse_trialComponentArn,
    disassociateTrialComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDisassociateTrialComponent' smart constructor.
data DisassociateTrialComponent = DisassociateTrialComponent'
  { -- | The name of the component to disassociate from the trial.
    trialComponentName :: Prelude.Text,
    -- | The name of the trial to disassociate from.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentName', 'disassociateTrialComponent_trialComponentName' - The name of the component to disassociate from the trial.
--
-- 'trialName', 'disassociateTrialComponent_trialName' - The name of the trial to disassociate from.
newDisassociateTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  -- | 'trialName'
  Prelude.Text ->
  DisassociateTrialComponent
newDisassociateTrialComponent
  pTrialComponentName_
  pTrialName_ =
    DisassociateTrialComponent'
      { trialComponentName =
          pTrialComponentName_,
        trialName = pTrialName_
      }

-- | The name of the component to disassociate from the trial.
disassociateTrialComponent_trialComponentName :: Lens.Lens' DisassociateTrialComponent Prelude.Text
disassociateTrialComponent_trialComponentName = Lens.lens (\DisassociateTrialComponent' {trialComponentName} -> trialComponentName) (\s@DisassociateTrialComponent' {} a -> s {trialComponentName = a} :: DisassociateTrialComponent)

-- | The name of the trial to disassociate from.
disassociateTrialComponent_trialName :: Lens.Lens' DisassociateTrialComponent Prelude.Text
disassociateTrialComponent_trialName = Lens.lens (\DisassociateTrialComponent' {trialName} -> trialName) (\s@DisassociateTrialComponent' {} a -> s {trialName = a} :: DisassociateTrialComponent)

instance Core.AWSRequest DisassociateTrialComponent where
  type
    AWSResponse DisassociateTrialComponent =
      DisassociateTrialComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateTrialComponentResponse'
            Prelude.<$> (x Data..?> "TrialArn")
            Prelude.<*> (x Data..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateTrialComponent where
  hashWithSalt _salt DisassociateTrialComponent' {..} =
    _salt
      `Prelude.hashWithSalt` trialComponentName
      `Prelude.hashWithSalt` trialName

instance Prelude.NFData DisassociateTrialComponent where
  rnf DisassociateTrialComponent' {..} =
    Prelude.rnf trialComponentName
      `Prelude.seq` Prelude.rnf trialName

instance Data.ToHeaders DisassociateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DisassociateTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateTrialComponent where
  toJSON DisassociateTrialComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrialComponentName" Data..= trialComponentName),
            Prelude.Just ("TrialName" Data..= trialName)
          ]
      )

instance Data.ToPath DisassociateTrialComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateTrialComponentResponse' smart constructor.
data DisassociateTrialComponentResponse = DisassociateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'disassociateTrialComponentResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'trialComponentArn', 'disassociateTrialComponentResponse_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'httpStatus', 'disassociateTrialComponentResponse_httpStatus' - The response's http status code.
newDisassociateTrialComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateTrialComponentResponse
newDisassociateTrialComponentResponse pHttpStatus_ =
  DisassociateTrialComponentResponse'
    { trialArn =
        Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
disassociateTrialComponentResponse_trialArn :: Lens.Lens' DisassociateTrialComponentResponse (Prelude.Maybe Prelude.Text)
disassociateTrialComponentResponse_trialArn = Lens.lens (\DisassociateTrialComponentResponse' {trialArn} -> trialArn) (\s@DisassociateTrialComponentResponse' {} a -> s {trialArn = a} :: DisassociateTrialComponentResponse)

-- | The Amazon Resource Name (ARN) of the trial component.
disassociateTrialComponentResponse_trialComponentArn :: Lens.Lens' DisassociateTrialComponentResponse (Prelude.Maybe Prelude.Text)
disassociateTrialComponentResponse_trialComponentArn = Lens.lens (\DisassociateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@DisassociateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: DisassociateTrialComponentResponse)

-- | The response's http status code.
disassociateTrialComponentResponse_httpStatus :: Lens.Lens' DisassociateTrialComponentResponse Prelude.Int
disassociateTrialComponentResponse_httpStatus = Lens.lens (\DisassociateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@DisassociateTrialComponentResponse' {} a -> s {httpStatus = a} :: DisassociateTrialComponentResponse)

instance
  Prelude.NFData
    DisassociateTrialComponentResponse
  where
  rnf DisassociateTrialComponentResponse' {..} =
    Prelude.rnf trialArn
      `Prelude.seq` Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf httpStatus
