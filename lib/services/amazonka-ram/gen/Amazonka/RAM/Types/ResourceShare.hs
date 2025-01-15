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
-- Module      : Amazonka.RAM.Types.ResourceShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShare where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types.ResourceShareFeatureSet
import Amazonka.RAM.Types.ResourceShareStatus
import Amazonka.RAM.Types.Tag

-- | Describes a resource share in RAM.
--
-- /See:/ 'newResourceShare' smart constructor.
data ResourceShare = ResourceShare'
  { -- | Indicates whether principals outside your organization in Organizations
    -- can be associated with a resource share.
    allowExternalPrincipals :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the resource share was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates how the resource share was created. Possible values include:
    --
    -- -   @CREATED_FROM_POLICY@ - Indicates that the resource share was
    --     created from an Identity and Access Management (IAM) resource-based
    --     permission policy attached to the resource. This type of resource
    --     share is visible only to the Amazon Web Services account that
    --     created it. You can\'t modify it in RAM unless you promote it. For
    --     more information, see PromoteResourceShareCreatedFromPolicy.
    --
    -- -   @PROMOTING_TO_STANDARD@ - The resource share is in the process of
    --     being promoted. For more information, see
    --     PromoteResourceShareCreatedFromPolicy.
    --
    -- -   @STANDARD@ - Indicates that the resource share was created in RAM
    --     using the console or APIs. These resource shares are visible to all
    --     principals you share the resource share with. You can modify these
    --     resource shares in RAM using the console or APIs.
    featureSet :: Prelude.Maybe ResourceShareFeatureSet,
    -- | The date and time when the resource share was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the resource share.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the resource share.
    owningAccountId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the resource share.
    status :: Prelude.Maybe ResourceShareStatus,
    -- | A message about the status of the resource share.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The tag key and value pairs attached to the resource share.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowExternalPrincipals', 'resourceShare_allowExternalPrincipals' - Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
--
-- 'creationTime', 'resourceShare_creationTime' - The date and time when the resource share was created.
--
-- 'featureSet', 'resourceShare_featureSet' - Indicates how the resource share was created. Possible values include:
--
-- -   @CREATED_FROM_POLICY@ - Indicates that the resource share was
--     created from an Identity and Access Management (IAM) resource-based
--     permission policy attached to the resource. This type of resource
--     share is visible only to the Amazon Web Services account that
--     created it. You can\'t modify it in RAM unless you promote it. For
--     more information, see PromoteResourceShareCreatedFromPolicy.
--
-- -   @PROMOTING_TO_STANDARD@ - The resource share is in the process of
--     being promoted. For more information, see
--     PromoteResourceShareCreatedFromPolicy.
--
-- -   @STANDARD@ - Indicates that the resource share was created in RAM
--     using the console or APIs. These resource shares are visible to all
--     principals you share the resource share with. You can modify these
--     resource shares in RAM using the console or APIs.
--
-- 'lastUpdatedTime', 'resourceShare_lastUpdatedTime' - The date and time when the resource share was last updated.
--
-- 'name', 'resourceShare_name' - The name of the resource share.
--
-- 'owningAccountId', 'resourceShare_owningAccountId' - The ID of the Amazon Web Services account that owns the resource share.
--
-- 'resourceShareArn', 'resourceShare_resourceShareArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share
--
-- 'status', 'resourceShare_status' - The current status of the resource share.
--
-- 'statusMessage', 'resourceShare_statusMessage' - A message about the status of the resource share.
--
-- 'tags', 'resourceShare_tags' - The tag key and value pairs attached to the resource share.
newResourceShare ::
  ResourceShare
newResourceShare =
  ResourceShare'
    { allowExternalPrincipals =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      owningAccountId = Prelude.Nothing,
      resourceShareArn = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
resourceShare_allowExternalPrincipals :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Bool)
resourceShare_allowExternalPrincipals = Lens.lens (\ResourceShare' {allowExternalPrincipals} -> allowExternalPrincipals) (\s@ResourceShare' {} a -> s {allowExternalPrincipals = a} :: ResourceShare)

-- | The date and time when the resource share was created.
resourceShare_creationTime :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.UTCTime)
resourceShare_creationTime = Lens.lens (\ResourceShare' {creationTime} -> creationTime) (\s@ResourceShare' {} a -> s {creationTime = a} :: ResourceShare) Prelude.. Lens.mapping Data._Time

-- | Indicates how the resource share was created. Possible values include:
--
-- -   @CREATED_FROM_POLICY@ - Indicates that the resource share was
--     created from an Identity and Access Management (IAM) resource-based
--     permission policy attached to the resource. This type of resource
--     share is visible only to the Amazon Web Services account that
--     created it. You can\'t modify it in RAM unless you promote it. For
--     more information, see PromoteResourceShareCreatedFromPolicy.
--
-- -   @PROMOTING_TO_STANDARD@ - The resource share is in the process of
--     being promoted. For more information, see
--     PromoteResourceShareCreatedFromPolicy.
--
-- -   @STANDARD@ - Indicates that the resource share was created in RAM
--     using the console or APIs. These resource shares are visible to all
--     principals you share the resource share with. You can modify these
--     resource shares in RAM using the console or APIs.
resourceShare_featureSet :: Lens.Lens' ResourceShare (Prelude.Maybe ResourceShareFeatureSet)
resourceShare_featureSet = Lens.lens (\ResourceShare' {featureSet} -> featureSet) (\s@ResourceShare' {} a -> s {featureSet = a} :: ResourceShare)

-- | The date and time when the resource share was last updated.
resourceShare_lastUpdatedTime :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.UTCTime)
resourceShare_lastUpdatedTime = Lens.lens (\ResourceShare' {lastUpdatedTime} -> lastUpdatedTime) (\s@ResourceShare' {} a -> s {lastUpdatedTime = a} :: ResourceShare) Prelude.. Lens.mapping Data._Time

-- | The name of the resource share.
resourceShare_name :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_name = Lens.lens (\ResourceShare' {name} -> name) (\s@ResourceShare' {} a -> s {name = a} :: ResourceShare)

-- | The ID of the Amazon Web Services account that owns the resource share.
resourceShare_owningAccountId :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_owningAccountId = Lens.lens (\ResourceShare' {owningAccountId} -> owningAccountId) (\s@ResourceShare' {} a -> s {owningAccountId = a} :: ResourceShare)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share
resourceShare_resourceShareArn :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_resourceShareArn = Lens.lens (\ResourceShare' {resourceShareArn} -> resourceShareArn) (\s@ResourceShare' {} a -> s {resourceShareArn = a} :: ResourceShare)

-- | The current status of the resource share.
resourceShare_status :: Lens.Lens' ResourceShare (Prelude.Maybe ResourceShareStatus)
resourceShare_status = Lens.lens (\ResourceShare' {status} -> status) (\s@ResourceShare' {} a -> s {status = a} :: ResourceShare)

-- | A message about the status of the resource share.
resourceShare_statusMessage :: Lens.Lens' ResourceShare (Prelude.Maybe Prelude.Text)
resourceShare_statusMessage = Lens.lens (\ResourceShare' {statusMessage} -> statusMessage) (\s@ResourceShare' {} a -> s {statusMessage = a} :: ResourceShare)

-- | The tag key and value pairs attached to the resource share.
resourceShare_tags :: Lens.Lens' ResourceShare (Prelude.Maybe [Tag])
resourceShare_tags = Lens.lens (\ResourceShare' {tags} -> tags) (\s@ResourceShare' {} a -> s {tags = a} :: ResourceShare) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceShare where
  parseJSON =
    Data.withObject
      "ResourceShare"
      ( \x ->
          ResourceShare'
            Prelude.<$> (x Data..:? "allowExternalPrincipals")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "featureSet")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owningAccountId")
            Prelude.<*> (x Data..:? "resourceShareArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceShare where
  hashWithSalt _salt ResourceShare' {..} =
    _salt
      `Prelude.hashWithSalt` allowExternalPrincipals
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` featureSet
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owningAccountId
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceShare where
  rnf ResourceShare' {..} =
    Prelude.rnf allowExternalPrincipals `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf featureSet `Prelude.seq`
          Prelude.rnf lastUpdatedTime `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf owningAccountId `Prelude.seq`
                Prelude.rnf resourceShareArn `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf statusMessage `Prelude.seq`
                      Prelude.rnf tags
