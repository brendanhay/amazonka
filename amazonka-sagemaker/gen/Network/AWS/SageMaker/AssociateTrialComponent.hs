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
-- Module      : Network.AWS.SageMaker.AssociateTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a trial component with a trial. A trial component can be
-- associated with multiple trials. To disassociate a trial component from
-- a trial, call the DisassociateTrialComponent API.
module Network.AWS.SageMaker.AssociateTrialComponent
  ( -- * Creating a Request
    AssociateTrialComponent (..),
    newAssociateTrialComponent,

    -- * Request Lenses
    associateTrialComponent_trialComponentName,
    associateTrialComponent_trialName,

    -- * Destructuring the Response
    AssociateTrialComponentResponse (..),
    newAssociateTrialComponentResponse,

    -- * Response Lenses
    associateTrialComponentResponse_trialArn,
    associateTrialComponentResponse_trialComponentArn,
    associateTrialComponentResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newAssociateTrialComponent' smart constructor.
data AssociateTrialComponent = AssociateTrialComponent'
  { -- | The name of the component to associated with the trial.
    trialComponentName :: Prelude.Text,
    -- | The name of the trial to associate with.
    trialName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentName', 'associateTrialComponent_trialComponentName' - The name of the component to associated with the trial.
--
-- 'trialName', 'associateTrialComponent_trialName' - The name of the trial to associate with.
newAssociateTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  -- | 'trialName'
  Prelude.Text ->
  AssociateTrialComponent
newAssociateTrialComponent
  pTrialComponentName_
  pTrialName_ =
    AssociateTrialComponent'
      { trialComponentName =
          pTrialComponentName_,
        trialName = pTrialName_
      }

-- | The name of the component to associated with the trial.
associateTrialComponent_trialComponentName :: Lens.Lens' AssociateTrialComponent Prelude.Text
associateTrialComponent_trialComponentName = Lens.lens (\AssociateTrialComponent' {trialComponentName} -> trialComponentName) (\s@AssociateTrialComponent' {} a -> s {trialComponentName = a} :: AssociateTrialComponent)

-- | The name of the trial to associate with.
associateTrialComponent_trialName :: Lens.Lens' AssociateTrialComponent Prelude.Text
associateTrialComponent_trialName = Lens.lens (\AssociateTrialComponent' {trialName} -> trialName) (\s@AssociateTrialComponent' {} a -> s {trialName = a} :: AssociateTrialComponent)

instance Prelude.AWSRequest AssociateTrialComponent where
  type
    Rs AssociateTrialComponent =
      AssociateTrialComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTrialComponentResponse'
            Prelude.<$> (x Prelude..?> "TrialArn")
            Prelude.<*> (x Prelude..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateTrialComponent

instance Prelude.NFData AssociateTrialComponent

instance Prelude.ToHeaders AssociateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.AssociateTrialComponent" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateTrialComponent where
  toJSON AssociateTrialComponent' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrialComponentName" Prelude..= trialComponentName),
            Prelude.Just ("TrialName" Prelude..= trialName)
          ]
      )

instance Prelude.ToPath AssociateTrialComponent where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateTrialComponentResponse' smart constructor.
data AssociateTrialComponentResponse = AssociateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'associateTrialComponentResponse_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'trialComponentArn', 'associateTrialComponentResponse_trialComponentArn' - The ARN of the trial component.
--
-- 'httpStatus', 'associateTrialComponentResponse_httpStatus' - The response's http status code.
newAssociateTrialComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateTrialComponentResponse
newAssociateTrialComponentResponse pHttpStatus_ =
  AssociateTrialComponentResponse'
    { trialArn =
        Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
associateTrialComponentResponse_trialArn :: Lens.Lens' AssociateTrialComponentResponse (Prelude.Maybe Prelude.Text)
associateTrialComponentResponse_trialArn = Lens.lens (\AssociateTrialComponentResponse' {trialArn} -> trialArn) (\s@AssociateTrialComponentResponse' {} a -> s {trialArn = a} :: AssociateTrialComponentResponse)

-- | The ARN of the trial component.
associateTrialComponentResponse_trialComponentArn :: Lens.Lens' AssociateTrialComponentResponse (Prelude.Maybe Prelude.Text)
associateTrialComponentResponse_trialComponentArn = Lens.lens (\AssociateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@AssociateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: AssociateTrialComponentResponse)

-- | The response's http status code.
associateTrialComponentResponse_httpStatus :: Lens.Lens' AssociateTrialComponentResponse Prelude.Int
associateTrialComponentResponse_httpStatus = Lens.lens (\AssociateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@AssociateTrialComponentResponse' {} a -> s {httpStatus = a} :: AssociateTrialComponentResponse)

instance
  Prelude.NFData
    AssociateTrialComponentResponse
