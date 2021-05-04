{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.DisassociateTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.SageMaker.DisassociateTrialComponent
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDisassociateTrialComponent' smart constructor.
data DisassociateTrialComponent = DisassociateTrialComponent'
  { -- | The name of the component to disassociate from the trial.
    trialComponentName :: Prelude.Text,
    -- | The name of the trial to disassociate from.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DisassociateTrialComponent
  where
  type
    Rs DisassociateTrialComponent =
      DisassociateTrialComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateTrialComponentResponse'
            Prelude.<$> (x Prelude..?> "TrialArn")
            Prelude.<*> (x Prelude..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateTrialComponent

instance Prelude.NFData DisassociateTrialComponent

instance Prelude.ToHeaders DisassociateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DisassociateTrialComponent" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateTrialComponent where
  toJSON DisassociateTrialComponent' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrialComponentName" Prelude..= trialComponentName),
            Prelude.Just ("TrialName" Prelude..= trialName)
          ]
      )

instance Prelude.ToPath DisassociateTrialComponent where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateTrialComponentResponse' smart constructor.
data DisassociateTrialComponentResponse = DisassociateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'trialComponentArn', 'disassociateTrialComponentResponse_trialComponentArn' - The ARN of the trial component.
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

-- | The ARN of the trial component.
disassociateTrialComponentResponse_trialComponentArn :: Lens.Lens' DisassociateTrialComponentResponse (Prelude.Maybe Prelude.Text)
disassociateTrialComponentResponse_trialComponentArn = Lens.lens (\DisassociateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@DisassociateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: DisassociateTrialComponentResponse)

-- | The response's http status code.
disassociateTrialComponentResponse_httpStatus :: Lens.Lens' DisassociateTrialComponentResponse Prelude.Int
disassociateTrialComponentResponse_httpStatus = Lens.lens (\DisassociateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@DisassociateTrialComponentResponse' {} a -> s {httpStatus = a} :: DisassociateTrialComponentResponse)

instance
  Prelude.NFData
    DisassociateTrialComponentResponse
