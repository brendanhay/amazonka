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
-- Module      : Amazonka.DevOpsGuru.Types.CostEstimationResourceCollectionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CostEstimationResourceCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.CloudFormationCostEstimationResourceCollectionFilter
import Amazonka.DevOpsGuru.Types.TagCostEstimationResourceCollectionFilter
import qualified Amazonka.Prelude as Prelude

-- | Information about a filter used to specify which Amazon Web Services
-- resources are analyzed to create a monthly DevOps Guru cost estimate.
-- For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
-- and
-- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
--
-- /See:/ 'newCostEstimationResourceCollectionFilter' smart constructor.
data CostEstimationResourceCollectionFilter = CostEstimationResourceCollectionFilter'
  { -- | An object that specifies the CloudFormation stack that defines the
    -- Amazon Web Services resources used to create a monthly estimate for
    -- DevOps Guru.
    cloudFormation :: Prelude.Maybe CloudFormationCostEstimationResourceCollectionFilter,
    -- | The Amazon Web Services tags used to filter the resource collection that
    -- is used for a cost estimate.
    --
    -- Tags help you identify and organize your Amazon Web Services resources.
    -- Many Amazon Web Services services support tagging, so you can assign the
    -- same tag to resources from different services to indicate that the
    -- resources are related. For example, you can assign the same tag to an
    -- Amazon DynamoDB table resource that you assign to an Lambda function.
    -- For more information about using tags, see the
    -- <https://d1.awsstatic.com/whitepapers/aws-tagging-best-practices.pdf Tagging best practices>
    -- whitepaper.
    --
    -- Each Amazon Web Services tag has two parts.
    --
    -- -   A tag /key/ (for example, @CostCenter@, @Environment@, @Project@, or
    --     @Secret@). Tag /keys/ are case-sensitive.
    --
    -- -   An optional field known as a tag /value/ (for example,
    --     @111122223333@, @Production@, or a team name). Omitting the tag
    --     /value/ is the same as using an empty string. Like tag /keys/, tag
    --     /values/ are case-sensitive.
    --
    -- Together these are known as /key/-/value/ pairs.
    --
    -- The string used for a /key/ in a tag that you use to define your
    -- resource coverage must begin with the prefix @Devops-guru-@. The tag
    -- /key/ might be @DevOps-Guru-deployment-application@ or
    -- @devops-guru-rds-application@. When you create a /key/, the case of
    -- characters in the /key/ can be whatever you choose. After you create a
    -- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
    -- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
    -- act as two different /keys/. Possible /key/\//value/ pairs in your
    -- application might be @Devops-Guru-production-application\/RDS@ or
    -- @Devops-Guru-production-application\/containers@.
    tags :: Prelude.Maybe [TagCostEstimationResourceCollectionFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostEstimationResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'costEstimationResourceCollectionFilter_cloudFormation' - An object that specifies the CloudFormation stack that defines the
-- Amazon Web Services resources used to create a monthly estimate for
-- DevOps Guru.
--
-- 'tags', 'costEstimationResourceCollectionFilter_tags' - The Amazon Web Services tags used to filter the resource collection that
-- is used for a cost estimate.
--
-- Tags help you identify and organize your Amazon Web Services resources.
-- Many Amazon Web Services services support tagging, so you can assign the
-- same tag to resources from different services to indicate that the
-- resources are related. For example, you can assign the same tag to an
-- Amazon DynamoDB table resource that you assign to an Lambda function.
-- For more information about using tags, see the
-- <https://d1.awsstatic.com/whitepapers/aws-tagging-best-practices.pdf Tagging best practices>
-- whitepaper.
--
-- Each Amazon Web Services tag has two parts.
--
-- -   A tag /key/ (for example, @CostCenter@, @Environment@, @Project@, or
--     @Secret@). Tag /keys/ are case-sensitive.
--
-- -   An optional field known as a tag /value/ (for example,
--     @111122223333@, @Production@, or a team name). Omitting the tag
--     /value/ is the same as using an empty string. Like tag /keys/, tag
--     /values/ are case-sensitive.
--
-- Together these are known as /key/-/value/ pairs.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
newCostEstimationResourceCollectionFilter ::
  CostEstimationResourceCollectionFilter
newCostEstimationResourceCollectionFilter =
  CostEstimationResourceCollectionFilter'
    { cloudFormation =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | An object that specifies the CloudFormation stack that defines the
-- Amazon Web Services resources used to create a monthly estimate for
-- DevOps Guru.
costEstimationResourceCollectionFilter_cloudFormation :: Lens.Lens' CostEstimationResourceCollectionFilter (Prelude.Maybe CloudFormationCostEstimationResourceCollectionFilter)
costEstimationResourceCollectionFilter_cloudFormation = Lens.lens (\CostEstimationResourceCollectionFilter' {cloudFormation} -> cloudFormation) (\s@CostEstimationResourceCollectionFilter' {} a -> s {cloudFormation = a} :: CostEstimationResourceCollectionFilter)

-- | The Amazon Web Services tags used to filter the resource collection that
-- is used for a cost estimate.
--
-- Tags help you identify and organize your Amazon Web Services resources.
-- Many Amazon Web Services services support tagging, so you can assign the
-- same tag to resources from different services to indicate that the
-- resources are related. For example, you can assign the same tag to an
-- Amazon DynamoDB table resource that you assign to an Lambda function.
-- For more information about using tags, see the
-- <https://d1.awsstatic.com/whitepapers/aws-tagging-best-practices.pdf Tagging best practices>
-- whitepaper.
--
-- Each Amazon Web Services tag has two parts.
--
-- -   A tag /key/ (for example, @CostCenter@, @Environment@, @Project@, or
--     @Secret@). Tag /keys/ are case-sensitive.
--
-- -   An optional field known as a tag /value/ (for example,
--     @111122223333@, @Production@, or a team name). Omitting the tag
--     /value/ is the same as using an empty string. Like tag /keys/, tag
--     /values/ are case-sensitive.
--
-- Together these are known as /key/-/value/ pairs.
--
-- The string used for a /key/ in a tag that you use to define your
-- resource coverage must begin with the prefix @Devops-guru-@. The tag
-- /key/ might be @DevOps-Guru-deployment-application@ or
-- @devops-guru-rds-application@. When you create a /key/, the case of
-- characters in the /key/ can be whatever you choose. After you create a
-- /key/, it is case-sensitive. For example, DevOps Guru works with a /key/
-- named @devops-guru-rds@ and a /key/ named @DevOps-Guru-RDS@, and these
-- act as two different /keys/. Possible /key/\//value/ pairs in your
-- application might be @Devops-Guru-production-application\/RDS@ or
-- @Devops-Guru-production-application\/containers@.
costEstimationResourceCollectionFilter_tags :: Lens.Lens' CostEstimationResourceCollectionFilter (Prelude.Maybe [TagCostEstimationResourceCollectionFilter])
costEstimationResourceCollectionFilter_tags = Lens.lens (\CostEstimationResourceCollectionFilter' {tags} -> tags) (\s@CostEstimationResourceCollectionFilter' {} a -> s {tags = a} :: CostEstimationResourceCollectionFilter) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    CostEstimationResourceCollectionFilter
  where
  parseJSON =
    Data.withObject
      "CostEstimationResourceCollectionFilter"
      ( \x ->
          CostEstimationResourceCollectionFilter'
            Prelude.<$> (x Data..:? "CloudFormation")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    CostEstimationResourceCollectionFilter
  where
  hashWithSalt
    _salt
    CostEstimationResourceCollectionFilter' {..} =
      _salt
        `Prelude.hashWithSalt` cloudFormation
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    CostEstimationResourceCollectionFilter
  where
  rnf CostEstimationResourceCollectionFilter' {..} =
    Prelude.rnf cloudFormation
      `Prelude.seq` Prelude.rnf tags

instance
  Data.ToJSON
    CostEstimationResourceCollectionFilter
  where
  toJSON CostEstimationResourceCollectionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudFormation" Data..=)
              Prelude.<$> cloudFormation,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )
