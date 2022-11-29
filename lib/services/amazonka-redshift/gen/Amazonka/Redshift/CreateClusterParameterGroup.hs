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
-- Module      : Amazonka.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Redshift parameter group.
--
-- Creating parameter groups is independent of creating clusters. You can
-- associate a cluster with a parameter group when you create the cluster.
-- You can also associate an existing cluster with a parameter group after
-- the cluster is created by using ModifyCluster.
--
-- Parameters in the parameter group define specific behavior that applies
-- to the databases you create on the cluster. For more information about
-- parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.CreateClusterParameterGroup
  ( -- * Creating a Request
    CreateClusterParameterGroup (..),
    newCreateClusterParameterGroup,

    -- * Request Lenses
    createClusterParameterGroup_tags,
    createClusterParameterGroup_parameterGroupName,
    createClusterParameterGroup_parameterGroupFamily,
    createClusterParameterGroup_description,

    -- * Destructuring the Response
    CreateClusterParameterGroupResponse (..),
    newCreateClusterParameterGroupResponse,

    -- * Response Lenses
    createClusterParameterGroupResponse_clusterParameterGroup,
    createClusterParameterGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateClusterParameterGroup' smart constructor.
data CreateClusterParameterGroup = CreateClusterParameterGroup'
  { -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the cluster parameter group.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters or hyphens
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- -   Must be unique withing your Amazon Web Services account.
    --
    -- This value is stored as a lower-case string.
    parameterGroupName :: Prelude.Text,
    -- | The Amazon Redshift engine version to which the cluster parameter group
    -- applies. The cluster engine version determines the set of parameters.
    --
    -- To get a list of valid parameter group family names, you can call
    -- DescribeClusterParameterGroups. By default, Amazon Redshift returns a
    -- list of all the parameter groups that are owned by your Amazon Web
    -- Services account, including the default parameter groups for each Amazon
    -- Redshift engine version. The parameter group family names associated
    -- with the default parameter groups provide you the valid values. For
    -- example, a valid family name is \"redshift-1.0\".
    parameterGroupFamily :: Prelude.Text,
    -- | A description of the parameter group.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createClusterParameterGroup_tags' - A list of tag instances.
--
-- 'parameterGroupName', 'createClusterParameterGroup_parameterGroupName' - The name of the cluster parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique withing your Amazon Web Services account.
--
-- This value is stored as a lower-case string.
--
-- 'parameterGroupFamily', 'createClusterParameterGroup_parameterGroupFamily' - The Amazon Redshift engine version to which the cluster parameter group
-- applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call
-- DescribeClusterParameterGroups. By default, Amazon Redshift returns a
-- list of all the parameter groups that are owned by your Amazon Web
-- Services account, including the default parameter groups for each Amazon
-- Redshift engine version. The parameter group family names associated
-- with the default parameter groups provide you the valid values. For
-- example, a valid family name is \"redshift-1.0\".
--
-- 'description', 'createClusterParameterGroup_description' - A description of the parameter group.
newCreateClusterParameterGroup ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  -- | 'parameterGroupFamily'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateClusterParameterGroup
newCreateClusterParameterGroup
  pParameterGroupName_
  pParameterGroupFamily_
  pDescription_ =
    CreateClusterParameterGroup'
      { tags =
          Prelude.Nothing,
        parameterGroupName = pParameterGroupName_,
        parameterGroupFamily = pParameterGroupFamily_,
        description = pDescription_
      }

-- | A list of tag instances.
createClusterParameterGroup_tags :: Lens.Lens' CreateClusterParameterGroup (Prelude.Maybe [Tag])
createClusterParameterGroup_tags = Lens.lens (\CreateClusterParameterGroup' {tags} -> tags) (\s@CreateClusterParameterGroup' {} a -> s {tags = a} :: CreateClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- -   Must be unique withing your Amazon Web Services account.
--
-- This value is stored as a lower-case string.
createClusterParameterGroup_parameterGroupName :: Lens.Lens' CreateClusterParameterGroup Prelude.Text
createClusterParameterGroup_parameterGroupName = Lens.lens (\CreateClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@CreateClusterParameterGroup' {} a -> s {parameterGroupName = a} :: CreateClusterParameterGroup)

-- | The Amazon Redshift engine version to which the cluster parameter group
-- applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call
-- DescribeClusterParameterGroups. By default, Amazon Redshift returns a
-- list of all the parameter groups that are owned by your Amazon Web
-- Services account, including the default parameter groups for each Amazon
-- Redshift engine version. The parameter group family names associated
-- with the default parameter groups provide you the valid values. For
-- example, a valid family name is \"redshift-1.0\".
createClusterParameterGroup_parameterGroupFamily :: Lens.Lens' CreateClusterParameterGroup Prelude.Text
createClusterParameterGroup_parameterGroupFamily = Lens.lens (\CreateClusterParameterGroup' {parameterGroupFamily} -> parameterGroupFamily) (\s@CreateClusterParameterGroup' {} a -> s {parameterGroupFamily = a} :: CreateClusterParameterGroup)

-- | A description of the parameter group.
createClusterParameterGroup_description :: Lens.Lens' CreateClusterParameterGroup Prelude.Text
createClusterParameterGroup_description = Lens.lens (\CreateClusterParameterGroup' {description} -> description) (\s@CreateClusterParameterGroup' {} a -> s {description = a} :: CreateClusterParameterGroup)

instance Core.AWSRequest CreateClusterParameterGroup where
  type
    AWSResponse CreateClusterParameterGroup =
      CreateClusterParameterGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateClusterParameterGroupResult"
      ( \s h x ->
          CreateClusterParameterGroupResponse'
            Prelude.<$> (x Core..@? "ClusterParameterGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateClusterParameterGroup where
  hashWithSalt _salt CreateClusterParameterGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` parameterGroupFamily
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateClusterParameterGroup where
  rnf CreateClusterParameterGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf parameterGroupFamily
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders CreateClusterParameterGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateClusterParameterGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateClusterParameterGroup where
  toQuery CreateClusterParameterGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateClusterParameterGroup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "ParameterGroupName" Core.=: parameterGroupName,
        "ParameterGroupFamily" Core.=: parameterGroupFamily,
        "Description" Core.=: description
      ]

-- | /See:/ 'newCreateClusterParameterGroupResponse' smart constructor.
data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse'
  { clusterParameterGroup :: Prelude.Maybe ClusterParameterGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterParameterGroup', 'createClusterParameterGroupResponse_clusterParameterGroup' - Undocumented member.
--
-- 'httpStatus', 'createClusterParameterGroupResponse_httpStatus' - The response's http status code.
newCreateClusterParameterGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterParameterGroupResponse
newCreateClusterParameterGroupResponse pHttpStatus_ =
  CreateClusterParameterGroupResponse'
    { clusterParameterGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createClusterParameterGroupResponse_clusterParameterGroup :: Lens.Lens' CreateClusterParameterGroupResponse (Prelude.Maybe ClusterParameterGroup)
createClusterParameterGroupResponse_clusterParameterGroup = Lens.lens (\CreateClusterParameterGroupResponse' {clusterParameterGroup} -> clusterParameterGroup) (\s@CreateClusterParameterGroupResponse' {} a -> s {clusterParameterGroup = a} :: CreateClusterParameterGroupResponse)

-- | The response's http status code.
createClusterParameterGroupResponse_httpStatus :: Lens.Lens' CreateClusterParameterGroupResponse Prelude.Int
createClusterParameterGroupResponse_httpStatus = Lens.lens (\CreateClusterParameterGroupResponse' {httpStatus} -> httpStatus) (\s@CreateClusterParameterGroupResponse' {} a -> s {httpStatus = a} :: CreateClusterParameterGroupResponse)

instance
  Prelude.NFData
    CreateClusterParameterGroupResponse
  where
  rnf CreateClusterParameterGroupResponse' {..} =
    Prelude.rnf clusterParameterGroup
      `Prelude.seq` Prelude.rnf httpStatus
