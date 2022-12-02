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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateResourceCollectionFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateResourceCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.UpdateCloudFormationCollectionFilter
import Amazonka.DevOpsGuru.Types.UpdateTagCollectionFilter
import qualified Amazonka.Prelude as Prelude

-- | Contains information used to update a collection of Amazon Web Services
-- resources.
--
-- /See:/ 'newUpdateResourceCollectionFilter' smart constructor.
data UpdateResourceCollectionFilter = UpdateResourceCollectionFilter'
  { -- | The updated Amazon Web Services tags used to filter the resources in the
    -- resource collection.
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
    tags :: Prelude.Maybe [UpdateTagCollectionFilter],
    -- | A collection of Amazon Web Services CloudFormation stacks. You can
    -- specify up to 500 Amazon Web Services CloudFormation stacks.
    cloudFormation :: Prelude.Maybe UpdateCloudFormationCollectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateResourceCollectionFilter_tags' - The updated Amazon Web Services tags used to filter the resources in the
-- resource collection.
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
--
-- 'cloudFormation', 'updateResourceCollectionFilter_cloudFormation' - A collection of Amazon Web Services CloudFormation stacks. You can
-- specify up to 500 Amazon Web Services CloudFormation stacks.
newUpdateResourceCollectionFilter ::
  UpdateResourceCollectionFilter
newUpdateResourceCollectionFilter =
  UpdateResourceCollectionFilter'
    { tags =
        Prelude.Nothing,
      cloudFormation = Prelude.Nothing
    }

-- | The updated Amazon Web Services tags used to filter the resources in the
-- resource collection.
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
updateResourceCollectionFilter_tags :: Lens.Lens' UpdateResourceCollectionFilter (Prelude.Maybe [UpdateTagCollectionFilter])
updateResourceCollectionFilter_tags = Lens.lens (\UpdateResourceCollectionFilter' {tags} -> tags) (\s@UpdateResourceCollectionFilter' {} a -> s {tags = a} :: UpdateResourceCollectionFilter) Prelude.. Lens.mapping Lens.coerced

-- | A collection of Amazon Web Services CloudFormation stacks. You can
-- specify up to 500 Amazon Web Services CloudFormation stacks.
updateResourceCollectionFilter_cloudFormation :: Lens.Lens' UpdateResourceCollectionFilter (Prelude.Maybe UpdateCloudFormationCollectionFilter)
updateResourceCollectionFilter_cloudFormation = Lens.lens (\UpdateResourceCollectionFilter' {cloudFormation} -> cloudFormation) (\s@UpdateResourceCollectionFilter' {} a -> s {cloudFormation = a} :: UpdateResourceCollectionFilter)

instance
  Prelude.Hashable
    UpdateResourceCollectionFilter
  where
  hashWithSalt
    _salt
    UpdateResourceCollectionFilter' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` cloudFormation

instance
  Prelude.NFData
    UpdateResourceCollectionFilter
  where
  rnf UpdateResourceCollectionFilter' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cloudFormation

instance Data.ToJSON UpdateResourceCollectionFilter where
  toJSON UpdateResourceCollectionFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("CloudFormation" Data..=)
              Prelude.<$> cloudFormation
          ]
      )
