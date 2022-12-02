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
-- Module      : Amazonka.Inspector2.Types.AwsEcrContainerAggregationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsEcrContainerAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | An aggregation of information about Amazon ECR containers.
--
-- /See:/ 'newAwsEcrContainerAggregationResponse' smart constructor.
data AwsEcrContainerAggregationResponse = AwsEcrContainerAggregationResponse'
  { -- | The number of finding by severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The container repository.
    repository :: Prelude.Maybe Prelude.Text,
    -- | The SHA value of the container image.
    imageSha :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the account that owns the
    -- container.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The container image stags.
    imageTags :: Prelude.Maybe [Prelude.Text],
    -- | The architecture of the container.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the container.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrContainerAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severityCounts', 'awsEcrContainerAggregationResponse_severityCounts' - The number of finding by severity.
--
-- 'repository', 'awsEcrContainerAggregationResponse_repository' - The container repository.
--
-- 'imageSha', 'awsEcrContainerAggregationResponse_imageSha' - The SHA value of the container image.
--
-- 'accountId', 'awsEcrContainerAggregationResponse_accountId' - The Amazon Web Services account ID of the account that owns the
-- container.
--
-- 'imageTags', 'awsEcrContainerAggregationResponse_imageTags' - The container image stags.
--
-- 'architecture', 'awsEcrContainerAggregationResponse_architecture' - The architecture of the container.
--
-- 'resourceId', 'awsEcrContainerAggregationResponse_resourceId' - The resource ID of the container.
newAwsEcrContainerAggregationResponse ::
  -- | 'resourceId'
  Prelude.Text ->
  AwsEcrContainerAggregationResponse
newAwsEcrContainerAggregationResponse pResourceId_ =
  AwsEcrContainerAggregationResponse'
    { severityCounts =
        Prelude.Nothing,
      repository = Prelude.Nothing,
      imageSha = Prelude.Nothing,
      accountId = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      architecture = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The number of finding by severity.
awsEcrContainerAggregationResponse_severityCounts :: Lens.Lens' AwsEcrContainerAggregationResponse (Prelude.Maybe SeverityCounts)
awsEcrContainerAggregationResponse_severityCounts = Lens.lens (\AwsEcrContainerAggregationResponse' {severityCounts} -> severityCounts) (\s@AwsEcrContainerAggregationResponse' {} a -> s {severityCounts = a} :: AwsEcrContainerAggregationResponse)

-- | The container repository.
awsEcrContainerAggregationResponse_repository :: Lens.Lens' AwsEcrContainerAggregationResponse (Prelude.Maybe Prelude.Text)
awsEcrContainerAggregationResponse_repository = Lens.lens (\AwsEcrContainerAggregationResponse' {repository} -> repository) (\s@AwsEcrContainerAggregationResponse' {} a -> s {repository = a} :: AwsEcrContainerAggregationResponse)

-- | The SHA value of the container image.
awsEcrContainerAggregationResponse_imageSha :: Lens.Lens' AwsEcrContainerAggregationResponse (Prelude.Maybe Prelude.Text)
awsEcrContainerAggregationResponse_imageSha = Lens.lens (\AwsEcrContainerAggregationResponse' {imageSha} -> imageSha) (\s@AwsEcrContainerAggregationResponse' {} a -> s {imageSha = a} :: AwsEcrContainerAggregationResponse)

-- | The Amazon Web Services account ID of the account that owns the
-- container.
awsEcrContainerAggregationResponse_accountId :: Lens.Lens' AwsEcrContainerAggregationResponse (Prelude.Maybe Prelude.Text)
awsEcrContainerAggregationResponse_accountId = Lens.lens (\AwsEcrContainerAggregationResponse' {accountId} -> accountId) (\s@AwsEcrContainerAggregationResponse' {} a -> s {accountId = a} :: AwsEcrContainerAggregationResponse)

-- | The container image stags.
awsEcrContainerAggregationResponse_imageTags :: Lens.Lens' AwsEcrContainerAggregationResponse (Prelude.Maybe [Prelude.Text])
awsEcrContainerAggregationResponse_imageTags = Lens.lens (\AwsEcrContainerAggregationResponse' {imageTags} -> imageTags) (\s@AwsEcrContainerAggregationResponse' {} a -> s {imageTags = a} :: AwsEcrContainerAggregationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The architecture of the container.
awsEcrContainerAggregationResponse_architecture :: Lens.Lens' AwsEcrContainerAggregationResponse (Prelude.Maybe Prelude.Text)
awsEcrContainerAggregationResponse_architecture = Lens.lens (\AwsEcrContainerAggregationResponse' {architecture} -> architecture) (\s@AwsEcrContainerAggregationResponse' {} a -> s {architecture = a} :: AwsEcrContainerAggregationResponse)

-- | The resource ID of the container.
awsEcrContainerAggregationResponse_resourceId :: Lens.Lens' AwsEcrContainerAggregationResponse Prelude.Text
awsEcrContainerAggregationResponse_resourceId = Lens.lens (\AwsEcrContainerAggregationResponse' {resourceId} -> resourceId) (\s@AwsEcrContainerAggregationResponse' {} a -> s {resourceId = a} :: AwsEcrContainerAggregationResponse)

instance
  Data.FromJSON
    AwsEcrContainerAggregationResponse
  where
  parseJSON =
    Data.withObject
      "AwsEcrContainerAggregationResponse"
      ( \x ->
          AwsEcrContainerAggregationResponse'
            Prelude.<$> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..:? "repository")
            Prelude.<*> (x Data..:? "imageSha")
            Prelude.<*> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "imageTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "architecture")
            Prelude.<*> (x Data..: "resourceId")
      )

instance
  Prelude.Hashable
    AwsEcrContainerAggregationResponse
  where
  hashWithSalt
    _salt
    AwsEcrContainerAggregationResponse' {..} =
      _salt `Prelude.hashWithSalt` severityCounts
        `Prelude.hashWithSalt` repository
        `Prelude.hashWithSalt` imageSha
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` imageTags
        `Prelude.hashWithSalt` architecture
        `Prelude.hashWithSalt` resourceId

instance
  Prelude.NFData
    AwsEcrContainerAggregationResponse
  where
  rnf AwsEcrContainerAggregationResponse' {..} =
    Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf imageSha
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf resourceId
