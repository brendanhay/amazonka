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
-- Module      : Amazonka.AuditManager.Types.CreateDelegationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.CreateDelegationRequest where

import Amazonka.AuditManager.Types.RoleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A collection of attributes that\'s used to create a delegation for an
-- assessment in Audit Manager.
--
-- /See:/ 'newCreateDelegationRequest' smart constructor.
data CreateDelegationRequest = CreateDelegationRequest'
  { -- | A comment that\'s related to the delegation request.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the control set.
    controlSetId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The type of customer persona.
    --
    -- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
    -- @RESOURCE_OWNER@.
    roleType :: Prelude.Maybe RoleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDelegationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'createDelegationRequest_comment' - A comment that\'s related to the delegation request.
--
-- 'controlSetId', 'createDelegationRequest_controlSetId' - The unique identifier for the control set.
--
-- 'roleArn', 'createDelegationRequest_roleArn' - The Amazon Resource Name (ARN) of the IAM role.
--
-- 'roleType', 'createDelegationRequest_roleType' - The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
newCreateDelegationRequest ::
  CreateDelegationRequest
newCreateDelegationRequest =
  CreateDelegationRequest'
    { comment = Prelude.Nothing,
      controlSetId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      roleType = Prelude.Nothing
    }

-- | A comment that\'s related to the delegation request.
createDelegationRequest_comment :: Lens.Lens' CreateDelegationRequest (Prelude.Maybe Prelude.Text)
createDelegationRequest_comment = Lens.lens (\CreateDelegationRequest' {comment} -> comment) (\s@CreateDelegationRequest' {} a -> s {comment = a} :: CreateDelegationRequest)

-- | The unique identifier for the control set.
createDelegationRequest_controlSetId :: Lens.Lens' CreateDelegationRequest (Prelude.Maybe Prelude.Text)
createDelegationRequest_controlSetId = Lens.lens (\CreateDelegationRequest' {controlSetId} -> controlSetId) (\s@CreateDelegationRequest' {} a -> s {controlSetId = a} :: CreateDelegationRequest)

-- | The Amazon Resource Name (ARN) of the IAM role.
createDelegationRequest_roleArn :: Lens.Lens' CreateDelegationRequest (Prelude.Maybe Prelude.Text)
createDelegationRequest_roleArn = Lens.lens (\CreateDelegationRequest' {roleArn} -> roleArn) (\s@CreateDelegationRequest' {} a -> s {roleArn = a} :: CreateDelegationRequest)

-- | The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
createDelegationRequest_roleType :: Lens.Lens' CreateDelegationRequest (Prelude.Maybe RoleType)
createDelegationRequest_roleType = Lens.lens (\CreateDelegationRequest' {roleType} -> roleType) (\s@CreateDelegationRequest' {} a -> s {roleType = a} :: CreateDelegationRequest)

instance Data.FromJSON CreateDelegationRequest where
  parseJSON =
    Data.withObject
      "CreateDelegationRequest"
      ( \x ->
          CreateDelegationRequest'
            Prelude.<$> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "controlSetId")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "roleType")
      )

instance Prelude.Hashable CreateDelegationRequest where
  hashWithSalt _salt CreateDelegationRequest' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` controlSetId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` roleType

instance Prelude.NFData CreateDelegationRequest where
  rnf CreateDelegationRequest' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf controlSetId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf roleType

instance Data.ToJSON CreateDelegationRequest where
  toJSON CreateDelegationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            ("controlSetId" Data..=) Prelude.<$> controlSetId,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("roleType" Data..=) Prelude.<$> roleType
          ]
      )
