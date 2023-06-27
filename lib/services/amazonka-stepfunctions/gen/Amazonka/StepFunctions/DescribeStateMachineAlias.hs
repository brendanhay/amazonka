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
-- Module      : Amazonka.StepFunctions.DescribeStateMachineAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html alias>.
--
-- __Related operations:__
--
-- -   CreateStateMachineAlias
--
-- -   ListStateMachineAliases
--
-- -   UpdateStateMachineAlias
--
-- -   DeleteStateMachineAlias
module Amazonka.StepFunctions.DescribeStateMachineAlias
  ( -- * Creating a Request
    DescribeStateMachineAlias (..),
    newDescribeStateMachineAlias,

    -- * Request Lenses
    describeStateMachineAlias_stateMachineAliasArn,

    -- * Destructuring the Response
    DescribeStateMachineAliasResponse (..),
    newDescribeStateMachineAliasResponse,

    -- * Response Lenses
    describeStateMachineAliasResponse_creationDate,
    describeStateMachineAliasResponse_description,
    describeStateMachineAliasResponse_name,
    describeStateMachineAliasResponse_routingConfiguration,
    describeStateMachineAliasResponse_stateMachineAliasArn,
    describeStateMachineAliasResponse_updateDate,
    describeStateMachineAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDescribeStateMachineAlias' smart constructor.
data DescribeStateMachineAlias = DescribeStateMachineAlias'
  { -- | The Amazon Resource Name (ARN) of the state machine alias.
    stateMachineAliasArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachineAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineAliasArn', 'describeStateMachineAlias_stateMachineAliasArn' - The Amazon Resource Name (ARN) of the state machine alias.
newDescribeStateMachineAlias ::
  -- | 'stateMachineAliasArn'
  Prelude.Text ->
  DescribeStateMachineAlias
newDescribeStateMachineAlias pStateMachineAliasArn_ =
  DescribeStateMachineAlias'
    { stateMachineAliasArn =
        pStateMachineAliasArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine alias.
describeStateMachineAlias_stateMachineAliasArn :: Lens.Lens' DescribeStateMachineAlias Prelude.Text
describeStateMachineAlias_stateMachineAliasArn = Lens.lens (\DescribeStateMachineAlias' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@DescribeStateMachineAlias' {} a -> s {stateMachineAliasArn = a} :: DescribeStateMachineAlias)

instance Core.AWSRequest DescribeStateMachineAlias where
  type
    AWSResponse DescribeStateMachineAlias =
      DescribeStateMachineAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStateMachineAliasResponse'
            Prelude.<$> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "routingConfiguration")
            Prelude.<*> (x Data..?> "stateMachineAliasArn")
            Prelude.<*> (x Data..?> "updateDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStateMachineAlias where
  hashWithSalt _salt DescribeStateMachineAlias' {..} =
    _salt `Prelude.hashWithSalt` stateMachineAliasArn

instance Prelude.NFData DescribeStateMachineAlias where
  rnf DescribeStateMachineAlias' {..} =
    Prelude.rnf stateMachineAliasArn

instance Data.ToHeaders DescribeStateMachineAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DescribeStateMachineAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStateMachineAlias where
  toJSON DescribeStateMachineAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "stateMachineAliasArn"
                  Data..= stateMachineAliasArn
              )
          ]
      )

instance Data.ToPath DescribeStateMachineAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStateMachineAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStateMachineAliasResponse' smart constructor.
data DescribeStateMachineAliasResponse = DescribeStateMachineAliasResponse'
  { -- | The date the state machine alias was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the alias.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the state machine alias.
    name :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration of the alias.
    routingConfiguration :: Prelude.Maybe (Prelude.NonEmpty RoutingConfigurationListItem),
    -- | The Amazon Resource Name (ARN) of the state machine alias.
    stateMachineAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The date the state machine alias was last updated.
    --
    -- For a newly created state machine, this is the same as the creation
    -- date.
    updateDate :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStateMachineAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'describeStateMachineAliasResponse_creationDate' - The date the state machine alias was created.
--
-- 'description', 'describeStateMachineAliasResponse_description' - A description of the alias.
--
-- 'name', 'describeStateMachineAliasResponse_name' - The name of the state machine alias.
--
-- 'routingConfiguration', 'describeStateMachineAliasResponse_routingConfiguration' - The routing configuration of the alias.
--
-- 'stateMachineAliasArn', 'describeStateMachineAliasResponse_stateMachineAliasArn' - The Amazon Resource Name (ARN) of the state machine alias.
--
-- 'updateDate', 'describeStateMachineAliasResponse_updateDate' - The date the state machine alias was last updated.
--
-- For a newly created state machine, this is the same as the creation
-- date.
--
-- 'httpStatus', 'describeStateMachineAliasResponse_httpStatus' - The response's http status code.
newDescribeStateMachineAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStateMachineAliasResponse
newDescribeStateMachineAliasResponse pHttpStatus_ =
  DescribeStateMachineAliasResponse'
    { creationDate =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      routingConfiguration = Prelude.Nothing,
      stateMachineAliasArn = Prelude.Nothing,
      updateDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the state machine alias was created.
describeStateMachineAliasResponse_creationDate :: Lens.Lens' DescribeStateMachineAliasResponse (Prelude.Maybe Prelude.UTCTime)
describeStateMachineAliasResponse_creationDate = Lens.lens (\DescribeStateMachineAliasResponse' {creationDate} -> creationDate) (\s@DescribeStateMachineAliasResponse' {} a -> s {creationDate = a} :: DescribeStateMachineAliasResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the alias.
describeStateMachineAliasResponse_description :: Lens.Lens' DescribeStateMachineAliasResponse (Prelude.Maybe Prelude.Text)
describeStateMachineAliasResponse_description = Lens.lens (\DescribeStateMachineAliasResponse' {description} -> description) (\s@DescribeStateMachineAliasResponse' {} a -> s {description = a} :: DescribeStateMachineAliasResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the state machine alias.
describeStateMachineAliasResponse_name :: Lens.Lens' DescribeStateMachineAliasResponse (Prelude.Maybe Prelude.Text)
describeStateMachineAliasResponse_name = Lens.lens (\DescribeStateMachineAliasResponse' {name} -> name) (\s@DescribeStateMachineAliasResponse' {} a -> s {name = a} :: DescribeStateMachineAliasResponse)

-- | The routing configuration of the alias.
describeStateMachineAliasResponse_routingConfiguration :: Lens.Lens' DescribeStateMachineAliasResponse (Prelude.Maybe (Prelude.NonEmpty RoutingConfigurationListItem))
describeStateMachineAliasResponse_routingConfiguration = Lens.lens (\DescribeStateMachineAliasResponse' {routingConfiguration} -> routingConfiguration) (\s@DescribeStateMachineAliasResponse' {} a -> s {routingConfiguration = a} :: DescribeStateMachineAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the state machine alias.
describeStateMachineAliasResponse_stateMachineAliasArn :: Lens.Lens' DescribeStateMachineAliasResponse (Prelude.Maybe Prelude.Text)
describeStateMachineAliasResponse_stateMachineAliasArn = Lens.lens (\DescribeStateMachineAliasResponse' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@DescribeStateMachineAliasResponse' {} a -> s {stateMachineAliasArn = a} :: DescribeStateMachineAliasResponse)

-- | The date the state machine alias was last updated.
--
-- For a newly created state machine, this is the same as the creation
-- date.
describeStateMachineAliasResponse_updateDate :: Lens.Lens' DescribeStateMachineAliasResponse (Prelude.Maybe Prelude.UTCTime)
describeStateMachineAliasResponse_updateDate = Lens.lens (\DescribeStateMachineAliasResponse' {updateDate} -> updateDate) (\s@DescribeStateMachineAliasResponse' {} a -> s {updateDate = a} :: DescribeStateMachineAliasResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeStateMachineAliasResponse_httpStatus :: Lens.Lens' DescribeStateMachineAliasResponse Prelude.Int
describeStateMachineAliasResponse_httpStatus = Lens.lens (\DescribeStateMachineAliasResponse' {httpStatus} -> httpStatus) (\s@DescribeStateMachineAliasResponse' {} a -> s {httpStatus = a} :: DescribeStateMachineAliasResponse)

instance
  Prelude.NFData
    DescribeStateMachineAliasResponse
  where
  rnf DescribeStateMachineAliasResponse' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routingConfiguration
      `Prelude.seq` Prelude.rnf stateMachineAliasArn
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf httpStatus
