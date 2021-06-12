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
-- Module      : Network.AWS.RDS.CopyDBClusterParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB cluster parameter group.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.CopyDBClusterParameterGroup
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCopyDBClusterParameterGroup' smart constructor.
data CopyDBClusterParameterGroup = CopyDBClusterParameterGroup'
  { tags :: Core.Maybe [Tag],
    -- | The identifier or Amazon Resource Name (ARN) for the source DB cluster
    -- parameter group. For information about creating an ARN, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Constraints:
    --
    -- -   Must specify a valid DB cluster parameter group.
    sourceDBClusterParameterGroupIdentifier :: Core.Text,
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
    targetDBClusterParameterGroupIdentifier :: Core.Text,
    -- | A description for the copied DB cluster parameter group.
    targetDBClusterParameterGroupDescription :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'targetDBClusterParameterGroupIdentifier'
  Core.Text ->
  -- | 'targetDBClusterParameterGroupDescription'
  Core.Text ->
  CopyDBClusterParameterGroup
newCopyDBClusterParameterGroup
  pSourceDBClusterParameterGroupIdentifier_
  pTargetDBClusterParameterGroupIdentifier_
  pTargetDBClusterParameterGroupDescription_ =
    CopyDBClusterParameterGroup'
      { tags = Core.Nothing,
        sourceDBClusterParameterGroupIdentifier =
          pSourceDBClusterParameterGroupIdentifier_,
        targetDBClusterParameterGroupIdentifier =
          pTargetDBClusterParameterGroupIdentifier_,
        targetDBClusterParameterGroupDescription =
          pTargetDBClusterParameterGroupDescription_
      }

-- | Undocumented member.
copyDBClusterParameterGroup_tags :: Lens.Lens' CopyDBClusterParameterGroup (Core.Maybe [Tag])
copyDBClusterParameterGroup_tags = Lens.lens (\CopyDBClusterParameterGroup' {tags} -> tags) (\s@CopyDBClusterParameterGroup' {} a -> s {tags = a} :: CopyDBClusterParameterGroup) Core.. Lens.mapping Lens._Coerce

-- | The identifier or Amazon Resource Name (ARN) for the source DB cluster
-- parameter group. For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon Aurora User Guide/.
--
-- Constraints:
--
-- -   Must specify a valid DB cluster parameter group.
copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Core.Text
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
copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Core.Text
copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier = Lens.lens (\CopyDBClusterParameterGroup' {targetDBClusterParameterGroupIdentifier} -> targetDBClusterParameterGroupIdentifier) (\s@CopyDBClusterParameterGroup' {} a -> s {targetDBClusterParameterGroupIdentifier = a} :: CopyDBClusterParameterGroup)

-- | A description for the copied DB cluster parameter group.
copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription :: Lens.Lens' CopyDBClusterParameterGroup Core.Text
copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription = Lens.lens (\CopyDBClusterParameterGroup' {targetDBClusterParameterGroupDescription} -> targetDBClusterParameterGroupDescription) (\s@CopyDBClusterParameterGroup' {} a -> s {targetDBClusterParameterGroupDescription = a} :: CopyDBClusterParameterGroup)

instance Core.AWSRequest CopyDBClusterParameterGroup where
  type
    AWSResponse CopyDBClusterParameterGroup =
      CopyDBClusterParameterGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CopyDBClusterParameterGroupResult"
      ( \s h x ->
          CopyDBClusterParameterGroupResponse'
            Core.<$> (x Core..@? "DBClusterParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopyDBClusterParameterGroup

instance Core.NFData CopyDBClusterParameterGroup

instance Core.ToHeaders CopyDBClusterParameterGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CopyDBClusterParameterGroup where
  toPath = Core.const "/"

instance Core.ToQuery CopyDBClusterParameterGroup where
  toQuery CopyDBClusterParameterGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CopyDBClusterParameterGroup" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "SourceDBClusterParameterGroupIdentifier"
          Core.=: sourceDBClusterParameterGroupIdentifier,
        "TargetDBClusterParameterGroupIdentifier"
          Core.=: targetDBClusterParameterGroupIdentifier,
        "TargetDBClusterParameterGroupDescription"
          Core.=: targetDBClusterParameterGroupDescription
      ]

-- | /See:/ 'newCopyDBClusterParameterGroupResponse' smart constructor.
data CopyDBClusterParameterGroupResponse = CopyDBClusterParameterGroupResponse'
  { dbClusterParameterGroup :: Core.Maybe DBClusterParameterGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CopyDBClusterParameterGroupResponse
newCopyDBClusterParameterGroupResponse pHttpStatus_ =
  CopyDBClusterParameterGroupResponse'
    { dbClusterParameterGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
copyDBClusterParameterGroupResponse_dbClusterParameterGroup :: Lens.Lens' CopyDBClusterParameterGroupResponse (Core.Maybe DBClusterParameterGroup)
copyDBClusterParameterGroupResponse_dbClusterParameterGroup = Lens.lens (\CopyDBClusterParameterGroupResponse' {dbClusterParameterGroup} -> dbClusterParameterGroup) (\s@CopyDBClusterParameterGroupResponse' {} a -> s {dbClusterParameterGroup = a} :: CopyDBClusterParameterGroupResponse)

-- | The response's http status code.
copyDBClusterParameterGroupResponse_httpStatus :: Lens.Lens' CopyDBClusterParameterGroupResponse Core.Int
copyDBClusterParameterGroupResponse_httpStatus = Lens.lens (\CopyDBClusterParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CopyDBClusterParameterGroupResponse' {} a -> s {httpStatus = a} :: CopyDBClusterParameterGroupResponse)

instance
  Core.NFData
    CopyDBClusterParameterGroupResponse
