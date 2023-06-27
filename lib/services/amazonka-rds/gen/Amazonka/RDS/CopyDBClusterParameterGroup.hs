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
-- Module      : Amazonka.RDS.CopyDBClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB cluster parameter group.
module Amazonka.RDS.CopyDBClusterParameterGroup
  ( -- * Creating a Request
    CopyDBClusterParameterGroup (..),
    newCopyDBClusterParameterGroup,

    -- * Request Lenses
    copyDBClusterParameterGroup_tags,
    copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription,

    -- * Destructuring the Response
    CopyDBClusterParameterGroupResponse (..),
    newCopyDBClusterParameterGroupResponse,

    -- * Response Lenses
    copyDBClusterParameterGroupResponse_dbClusterParameterGroup,
    copyDBClusterParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyDBClusterParameterGroup' smart constructor.
data CopyDBClusterParameterGroup = CopyDBClusterParameterGroup'
  { tags :: Prelude.Maybe [Tag],
    -- | The identifier or Amazon Resource Name (ARN) for the source DB cluster
    -- parameter group. For information about creating an ARN, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Constraints:
    --
    -- -   Must specify a valid DB cluster parameter group.
    sourceDBClusterParameterGroupIdentifier :: Prelude.Text,
    -- | The identifier for the copied DB cluster parameter group.
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
    -- Example: @my-cluster-param-group1@
    targetDBClusterParameterGroupIdentifier :: Prelude.Text,
    -- | A description for the copied DB cluster parameter group.
    targetDBClusterParameterGroupDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'copyDBClusterParameterGroup_tags' - Undocumented member.
--
-- 'sourceDBClusterParameterGroupIdentifier', 'copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier' - The identifier or Amazon Resource Name (ARN) for the source DB cluster
-- parameter group. For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon Aurora User Guide/.
--
-- Constraints:
--
-- -   Must specify a valid DB cluster parameter group.
--
-- 'targetDBClusterParameterGroupIdentifier', 'copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier' - The identifier for the copied DB cluster parameter group.
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
-- Example: @my-cluster-param-group1@
--
-- 'targetDBClusterParameterGroupDescription', 'copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription' - A description for the copied DB cluster parameter group.
newCopyDBClusterParameterGroup ::
  -- | 'sourceDBClusterParameterGroupIdentifier'
  Prelude.Text ->
  -- | 'targetDBClusterParameterGroupIdentifier'
  Prelude.Text ->
  -- | 'targetDBClusterParameterGroupDescription'
  Prelude.Text ->
  CopyDBClusterParameterGroup
newCopyDBClusterParameterGroup
  pSourceDBClusterParameterGroupIdentifier_
  pTargetDBClusterParameterGroupIdentifier_
  pTargetDBClusterParameterGroupDescription_ =
    CopyDBClusterParameterGroup'
      { tags =
          Prelude.Nothing,
        sourceDBClusterParameterGroupIdentifier =
          pSourceDBClusterParameterGroupIdentifier_,
        targetDBClusterParameterGroupIdentifier =
          pTargetDBClusterParameterGroupIdentifier_,
        targetDBClusterParameterGroupDescription =
          pTargetDBClusterParameterGroupDescription_
      }

-- | Undocumented member.
copyDBClusterParameterGroup_tags :: Lens.Lens' CopyDBClusterParameterGroup (Prelude.Maybe [Tag])
copyDBClusterParameterGroup_tags = Lens.lens (\CopyDBClusterParameterGroup' {tags} -> tags) (\s@CopyDBClusterParameterGroup' {} a -> s {tags = a} :: CopyDBClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The identifier or Amazon Resource Name (ARN) for the source DB cluster
-- parameter group. For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon Aurora User Guide/.
--
-- Constraints:
--
-- -   Must specify a valid DB cluster parameter group.
copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Prelude.Text
copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier = Lens.lens (\CopyDBClusterParameterGroup' {sourceDBClusterParameterGroupIdentifier} -> sourceDBClusterParameterGroupIdentifier) (\s@CopyDBClusterParameterGroup' {} a -> s {sourceDBClusterParameterGroupIdentifier = a} :: CopyDBClusterParameterGroup)

-- | The identifier for the copied DB cluster parameter group.
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
-- Example: @my-cluster-param-group1@
copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Prelude.Text
copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier = Lens.lens (\CopyDBClusterParameterGroup' {targetDBClusterParameterGroupIdentifier} -> targetDBClusterParameterGroupIdentifier) (\s@CopyDBClusterParameterGroup' {} a -> s {targetDBClusterParameterGroupIdentifier = a} :: CopyDBClusterParameterGroup)

-- | A description for the copied DB cluster parameter group.
copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription :: Lens.Lens' CopyDBClusterParameterGroup Prelude.Text
copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription = Lens.lens (\CopyDBClusterParameterGroup' {targetDBClusterParameterGroupDescription} -> targetDBClusterParameterGroupDescription) (\s@CopyDBClusterParameterGroup' {} a -> s {targetDBClusterParameterGroupDescription = a} :: CopyDBClusterParameterGroup)

instance Core.AWSRequest CopyDBClusterParameterGroup where
  type
    AWSResponse CopyDBClusterParameterGroup =
      CopyDBClusterParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopyDBClusterParameterGroupResult"
      ( \s h x ->
          CopyDBClusterParameterGroupResponse'
            Prelude.<$> (x Data..@? "DBClusterParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyDBClusterParameterGroup where
  hashWithSalt _salt CopyDBClusterParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceDBClusterParameterGroupIdentifier
      `Prelude.hashWithSalt` targetDBClusterParameterGroupIdentifier
      `Prelude.hashWithSalt` targetDBClusterParameterGroupDescription

instance Prelude.NFData CopyDBClusterParameterGroup where
  rnf CopyDBClusterParameterGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceDBClusterParameterGroupIdentifier
      `Prelude.seq` Prelude.rnf targetDBClusterParameterGroupIdentifier
      `Prelude.seq` Prelude.rnf targetDBClusterParameterGroupDescription

instance Data.ToHeaders CopyDBClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyDBClusterParameterGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyDBClusterParameterGroup where
  toQuery CopyDBClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CopyDBClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "SourceDBClusterParameterGroupIdentifier"
          Data.=: sourceDBClusterParameterGroupIdentifier,
        "TargetDBClusterParameterGroupIdentifier"
          Data.=: targetDBClusterParameterGroupIdentifier,
        "TargetDBClusterParameterGroupDescription"
          Data.=: targetDBClusterParameterGroupDescription
      ]

-- | /See:/ 'newCopyDBClusterParameterGroupResponse' smart constructor.
data CopyDBClusterParameterGroupResponse = CopyDBClusterParameterGroupResponse'
  { dbClusterParameterGroup :: Prelude.Maybe DBClusterParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyDBClusterParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroup', 'copyDBClusterParameterGroupResponse_dbClusterParameterGroup' - Undocumented member.
--
-- 'httpStatus', 'copyDBClusterParameterGroupResponse_httpStatus' - The response's http status code.
newCopyDBClusterParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyDBClusterParameterGroupResponse
newCopyDBClusterParameterGroupResponse pHttpStatus_ =
  CopyDBClusterParameterGroupResponse'
    { dbClusterParameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyDBClusterParameterGroupResponse_dbClusterParameterGroup :: Lens.Lens' CopyDBClusterParameterGroupResponse (Prelude.Maybe DBClusterParameterGroup)
copyDBClusterParameterGroupResponse_dbClusterParameterGroup = Lens.lens (\CopyDBClusterParameterGroupResponse' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@CopyDBClusterParameterGroupResponse' {} a -> s {dbClusterParameterGroup = a} :: CopyDBClusterParameterGroupResponse)

-- | The response's http status code.
copyDBClusterParameterGroupResponse_httpStatus :: Lens.Lens' CopyDBClusterParameterGroupResponse Prelude.Int
copyDBClusterParameterGroupResponse_httpStatus = Lens.lens (\CopyDBClusterParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CopyDBClusterParameterGroupResponse' {} a -> s {httpStatus = a} :: CopyDBClusterParameterGroupResponse)

instance
  Prelude.NFData
    CopyDBClusterParameterGroupResponse
  where
  rnf CopyDBClusterParameterGroupResponse' {..} =
    Prelude.rnf dbClusterParameterGroup
      `Prelude.seq` Prelude.rnf httpStatus
