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
-- Module      : Amazonka.RDS.CopyDBParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB parameter group.
module Amazonka.RDS.CopyDBParameterGroup
  ( -- * Creating a Request
    CopyDBParameterGroup (..),
    newCopyDBParameterGroup,

    -- * Request Lenses
    copyDBParameterGroup_tags,
    copyDBParameterGroup_sourceDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupIdentifier,
    copyDBParameterGroup_targetDBParameterGroupDescription,

    -- * Destructuring the Response
    CopyDBParameterGroupResponse (..),
    newCopyDBParameterGroupResponse,

    -- * Response Lenses
    copyDBParameterGroupResponse_dbParameterGroup,
    copyDBParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCopyDBParameterGroup' smart constructor.
data CopyDBParameterGroup = CopyDBParameterGroup'
  { tags :: Prelude.Maybe [Tag],
    -- | The identifier or ARN for the source DB parameter group. For information
    -- about creating an ARN, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints:
    --
    -- -   Must specify a valid DB parameter group.
    sourceDBParameterGroupIdentifier :: Prelude.Text,
    -- | The identifier for the copied DB parameter group.
    --
    -- Constraints:
    --
    -- -   Can\'t be null, empty, or blank
    --
    -- -   Must contain from 1 to 255 letters, numbers, or hyphens
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-db-parameter-group@
    targetDBParameterGroupIdentifier :: Prelude.Text,
    -- | A description for the copied DB parameter group.
    targetDBParameterGroupDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'copyDBParameterGroup_tags' - Undocumented member.
--
-- 'sourceDBParameterGroupIdentifier', 'copyDBParameterGroup_sourceDBParameterGroupIdentifier' - The identifier or ARN for the source DB parameter group. For information
-- about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- Constraints:
--
-- -   Must specify a valid DB parameter group.
--
-- 'targetDBParameterGroupIdentifier', 'copyDBParameterGroup_targetDBParameterGroupIdentifier' - The identifier for the copied DB parameter group.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-db-parameter-group@
--
-- 'targetDBParameterGroupDescription', 'copyDBParameterGroup_targetDBParameterGroupDescription' - A description for the copied DB parameter group.
newCopyDBParameterGroup ::
  -- | 'sourceDBParameterGroupIdentifier'
  Prelude.Text ->
  -- | 'targetDBParameterGroupIdentifier'
  Prelude.Text ->
  -- | 'targetDBParameterGroupDescription'
  Prelude.Text ->
  CopyDBParameterGroup
newCopyDBParameterGroup
  pSourceDBParameterGroupIdentifier_
  pTargetDBParameterGroupIdentifier_
  pTargetDBParameterGroupDescription_ =
    CopyDBParameterGroup'
      { tags = Prelude.Nothing,
        sourceDBParameterGroupIdentifier =
          pSourceDBParameterGroupIdentifier_,
        targetDBParameterGroupIdentifier =
          pTargetDBParameterGroupIdentifier_,
        targetDBParameterGroupDescription =
          pTargetDBParameterGroupDescription_
      }

-- | Undocumented member.
copyDBParameterGroup_tags :: Lens.Lens' CopyDBParameterGroup (Prelude.Maybe [Tag])
copyDBParameterGroup_tags = Lens.lens (\CopyDBParameterGroup' {tags} -> tags) (\s@CopyDBParameterGroup' {} a -> s {tags = a} :: CopyDBParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The identifier or ARN for the source DB parameter group. For information
-- about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- Constraints:
--
-- -   Must specify a valid DB parameter group.
copyDBParameterGroup_sourceDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Prelude.Text
copyDBParameterGroup_sourceDBParameterGroupIdentifier = Lens.lens (\CopyDBParameterGroup' {sourceDBParameterGroupIdentifier} -> sourceDBParameterGroupIdentifier) (\s@CopyDBParameterGroup' {} a -> s {sourceDBParameterGroupIdentifier = a} :: CopyDBParameterGroup)

-- | The identifier for the copied DB parameter group.
--
-- Constraints:
--
-- -   Can\'t be null, empty, or blank
--
-- -   Must contain from 1 to 255 letters, numbers, or hyphens
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-db-parameter-group@
copyDBParameterGroup_targetDBParameterGroupIdentifier :: Lens.Lens' CopyDBParameterGroup Prelude.Text
copyDBParameterGroup_targetDBParameterGroupIdentifier = Lens.lens (\CopyDBParameterGroup' {targetDBParameterGroupIdentifier} -> targetDBParameterGroupIdentifier) (\s@CopyDBParameterGroup' {} a -> s {targetDBParameterGroupIdentifier = a} :: CopyDBParameterGroup)

-- | A description for the copied DB parameter group.
copyDBParameterGroup_targetDBParameterGroupDescription :: Lens.Lens' CopyDBParameterGroup Prelude.Text
copyDBParameterGroup_targetDBParameterGroupDescription = Lens.lens (\CopyDBParameterGroup' {targetDBParameterGroupDescription} -> targetDBParameterGroupDescription) (\s@CopyDBParameterGroup' {} a -> s {targetDBParameterGroupDescription = a} :: CopyDBParameterGroup)

instance Core.AWSRequest CopyDBParameterGroup where
  type
    AWSResponse CopyDBParameterGroup =
      CopyDBParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopyDBParameterGroupResult"
      ( \s h x ->
          CopyDBParameterGroupResponse'
            Prelude.<$> (x Data..@? "DBParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBParameterGroup where
  hashWithSalt _salt CopyDBParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceDBParameterGroupIdentifier
      `Prelude.hashWithSalt` targetDBParameterGroupIdentifier
      `Prelude.hashWithSalt` targetDBParameterGroupDescription

instance Prelude.NFData CopyDBParameterGroup where
  rnf CopyDBParameterGroup' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf sourceDBParameterGroupIdentifier `Prelude.seq`
        Prelude.rnf targetDBParameterGroupIdentifier `Prelude.seq`
          Prelude.rnf targetDBParameterGroupDescription

instance Data.ToHeaders CopyDBParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyDBParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyDBParameterGroup where
  toQuery CopyDBParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyDBParameterGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "SourceDBParameterGroupIdentifier"
          Data.=: sourceDBParameterGroupIdentifier,
        "TargetDBParameterGroupIdentifier"
          Data.=: targetDBParameterGroupIdentifier,
        "TargetDBParameterGroupDescription"
          Data.=: targetDBParameterGroupDescription
      ]

-- | /See:/ 'newCopyDBParameterGroupResponse' smart constructor.
data CopyDBParameterGroupResponse = CopyDBParameterGroupResponse'
  { dbParameterGroup :: Prelude.Maybe DBParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroup', 'copyDBParameterGroupResponse_dbParameterGroup' - Undocumented member.
--
-- 'httpStatus', 'copyDBParameterGroupResponse_httpStatus' - The response's http status code.
newCopyDBParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyDBParameterGroupResponse
newCopyDBParameterGroupResponse pHttpStatus_ =
  CopyDBParameterGroupResponse'
    { dbParameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyDBParameterGroupResponse_dbParameterGroup :: Lens.Lens' CopyDBParameterGroupResponse (Prelude.Maybe DBParameterGroup)
copyDBParameterGroupResponse_dbParameterGroup = Lens.lens (\CopyDBParameterGroupResponse' {dbParameterGroup} -> dbParameterGroup) (\s@CopyDBParameterGroupResponse' {} a -> s {dbParameterGroup = a} :: CopyDBParameterGroupResponse)

-- | The response's http status code.
copyDBParameterGroupResponse_httpStatus :: Lens.Lens' CopyDBParameterGroupResponse Prelude.Int
copyDBParameterGroupResponse_httpStatus = Lens.lens (\CopyDBParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CopyDBParameterGroupResponse' {} a -> s {httpStatus = a} :: CopyDBParameterGroupResponse)

instance Prelude.NFData CopyDBParameterGroupResponse where
  rnf CopyDBParameterGroupResponse' {..} =
    Prelude.rnf dbParameterGroup `Prelude.seq`
      Prelude.rnf httpStatus
