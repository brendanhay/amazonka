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
-- Module      : Amazonka.Personalize.CreateDatasetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty dataset group. A dataset group is a container for
-- Amazon Personalize resources. A dataset group can contain at most three
-- datasets, one for each type of dataset:
--
-- -   Interactions
--
-- -   Items
--
-- -   Users
--
-- A dataset group can be a Domain dataset group, where you specify a
-- domain and use pre-configured resources like recommenders, or a Custom
-- dataset group, where you use custom resources, such as a solution with a
-- solution version, that you deploy with a campaign. If you start with a
-- Domain dataset group, you can still add custom resources such as
-- solutions and solution versions trained with recipes for custom use
-- cases and deployed with campaigns.
--
-- A dataset group can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING
--
-- To get the status of the dataset group, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>.
-- If the status shows as CREATE FAILED, the response includes a
-- @failureReason@ key, which describes why the creation failed.
--
-- You must wait until the @status@ of the dataset group is @ACTIVE@ before
-- adding a dataset to the group.
--
-- You can specify an Key Management Service (KMS) key to encrypt the
-- datasets in the group. If you specify a KMS key, you must also include
-- an Identity and Access Management (IAM) role that has permission to
-- access the key.
--
-- __APIs that require a dataset group ARN in the request__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDataset.html CreateDataset>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateEventTracker.html CreateEventTracker>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListDatasetGroups.html ListDatasetGroups>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DeleteDatasetGroup.html DeleteDatasetGroup>
module Amazonka.Personalize.CreateDatasetGroup
  ( -- * Creating a Request
    CreateDatasetGroup (..),
    newCreateDatasetGroup,

    -- * Request Lenses
    createDatasetGroup_domain,
    createDatasetGroup_kmsKeyArn,
    createDatasetGroup_roleArn,
    createDatasetGroup_tags,
    createDatasetGroup_name,

    -- * Destructuring the Response
    CreateDatasetGroupResponse (..),
    newCreateDatasetGroupResponse,

    -- * Response Lenses
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_domain,
    createDatasetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDatasetGroup' smart constructor.
data CreateDatasetGroup = CreateDatasetGroup'
  { -- | The domain of the dataset group. Specify a domain to create a Domain
    -- dataset group. The domain you specify determines the default schemas for
    -- datasets and the use cases available for recommenders. If you don\'t
    -- specify a domain, you create a Custom dataset group with solution
    -- versions that you deploy with a campaign.
    domain :: Prelude.Maybe Domain,
    -- | The Amazon Resource Name (ARN) of a Key Management Service (KMS) key
    -- used to encrypt the datasets.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Identity and Access Management (IAM) role that has
    -- permissions to access the Key Management Service (KMS) key. Supplying an
    -- IAM role is only valid when also specifying a KMS key.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the dataset group.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the new dataset group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createDatasetGroup_domain' - The domain of the dataset group. Specify a domain to create a Domain
-- dataset group. The domain you specify determines the default schemas for
-- datasets and the use cases available for recommenders. If you don\'t
-- specify a domain, you create a Custom dataset group with solution
-- versions that you deploy with a campaign.
--
-- 'kmsKeyArn', 'createDatasetGroup_kmsKeyArn' - The Amazon Resource Name (ARN) of a Key Management Service (KMS) key
-- used to encrypt the datasets.
--
-- 'roleArn', 'createDatasetGroup_roleArn' - The ARN of the Identity and Access Management (IAM) role that has
-- permissions to access the Key Management Service (KMS) key. Supplying an
-- IAM role is only valid when also specifying a KMS key.
--
-- 'tags', 'createDatasetGroup_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the dataset group.
--
-- 'name', 'createDatasetGroup_name' - The name for the new dataset group.
newCreateDatasetGroup ::
  -- | 'name'
  Prelude.Text ->
  CreateDatasetGroup
newCreateDatasetGroup pName_ =
  CreateDatasetGroup'
    { domain = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The domain of the dataset group. Specify a domain to create a Domain
-- dataset group. The domain you specify determines the default schemas for
-- datasets and the use cases available for recommenders. If you don\'t
-- specify a domain, you create a Custom dataset group with solution
-- versions that you deploy with a campaign.
createDatasetGroup_domain :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe Domain)
createDatasetGroup_domain = Lens.lens (\CreateDatasetGroup' {domain} -> domain) (\s@CreateDatasetGroup' {} a -> s {domain = a} :: CreateDatasetGroup)

-- | The Amazon Resource Name (ARN) of a Key Management Service (KMS) key
-- used to encrypt the datasets.
createDatasetGroup_kmsKeyArn :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe Prelude.Text)
createDatasetGroup_kmsKeyArn = Lens.lens (\CreateDatasetGroup' {kmsKeyArn} -> kmsKeyArn) (\s@CreateDatasetGroup' {} a -> s {kmsKeyArn = a} :: CreateDatasetGroup)

-- | The ARN of the Identity and Access Management (IAM) role that has
-- permissions to access the Key Management Service (KMS) key. Supplying an
-- IAM role is only valid when also specifying a KMS key.
createDatasetGroup_roleArn :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe Prelude.Text)
createDatasetGroup_roleArn = Lens.lens (\CreateDatasetGroup' {roleArn} -> roleArn) (\s@CreateDatasetGroup' {} a -> s {roleArn = a} :: CreateDatasetGroup)

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the dataset group.
createDatasetGroup_tags :: Lens.Lens' CreateDatasetGroup (Prelude.Maybe [Tag])
createDatasetGroup_tags = Lens.lens (\CreateDatasetGroup' {tags} -> tags) (\s@CreateDatasetGroup' {} a -> s {tags = a} :: CreateDatasetGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name for the new dataset group.
createDatasetGroup_name :: Lens.Lens' CreateDatasetGroup Prelude.Text
createDatasetGroup_name = Lens.lens (\CreateDatasetGroup' {name} -> name) (\s@CreateDatasetGroup' {} a -> s {name = a} :: CreateDatasetGroup)

instance Core.AWSRequest CreateDatasetGroup where
  type
    AWSResponse CreateDatasetGroup =
      CreateDatasetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDatasetGroupResponse'
            Prelude.<$> (x Data..?> "datasetGroupArn")
            Prelude.<*> (x Data..?> "domain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatasetGroup where
  hashWithSalt _salt CreateDatasetGroup' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateDatasetGroup where
  rnf CreateDatasetGroup' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateDatasetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateDatasetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatasetGroup where
  toJSON CreateDatasetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domain" Data..=) Prelude.<$> domain,
            ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateDatasetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDatasetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatasetGroupResponse' smart constructor.
data CreateDatasetGroupResponse = CreateDatasetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the new dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The domain for the new Domain dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatasetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'createDatasetGroupResponse_datasetGroupArn' - The Amazon Resource Name (ARN) of the new dataset group.
--
-- 'domain', 'createDatasetGroupResponse_domain' - The domain for the new Domain dataset group.
--
-- 'httpStatus', 'createDatasetGroupResponse_httpStatus' - The response's http status code.
newCreateDatasetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatasetGroupResponse
newCreateDatasetGroupResponse pHttpStatus_ =
  CreateDatasetGroupResponse'
    { datasetGroupArn =
        Prelude.Nothing,
      domain = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new dataset group.
createDatasetGroupResponse_datasetGroupArn :: Lens.Lens' CreateDatasetGroupResponse (Prelude.Maybe Prelude.Text)
createDatasetGroupResponse_datasetGroupArn = Lens.lens (\CreateDatasetGroupResponse' {datasetGroupArn} -> datasetGroupArn) (\s@CreateDatasetGroupResponse' {} a -> s {datasetGroupArn = a} :: CreateDatasetGroupResponse)

-- | The domain for the new Domain dataset group.
createDatasetGroupResponse_domain :: Lens.Lens' CreateDatasetGroupResponse (Prelude.Maybe Domain)
createDatasetGroupResponse_domain = Lens.lens (\CreateDatasetGroupResponse' {domain} -> domain) (\s@CreateDatasetGroupResponse' {} a -> s {domain = a} :: CreateDatasetGroupResponse)

-- | The response's http status code.
createDatasetGroupResponse_httpStatus :: Lens.Lens' CreateDatasetGroupResponse Prelude.Int
createDatasetGroupResponse_httpStatus = Lens.lens (\CreateDatasetGroupResponse' {httpStatus} -> httpStatus) (\s@CreateDatasetGroupResponse' {} a -> s {httpStatus = a} :: CreateDatasetGroupResponse)

instance Prelude.NFData CreateDatasetGroupResponse where
  rnf CreateDatasetGroupResponse' {..} =
    Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf httpStatus
