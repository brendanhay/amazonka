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
-- Module      : Amazonka.DevOpsGuru.Types.ResourceCollectionFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ResourceCollectionFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.CloudFormationCollectionFilter
import Amazonka.DevOpsGuru.Types.TagCollectionFilter
import qualified Amazonka.Prelude as Prelude

-- | Information about a filter used to specify which Amazon Web Services
-- resources are analyzed for anomalous behavior by DevOps Guru.
--
-- /See:/ 'newResourceCollectionFilter' smart constructor.
data ResourceCollectionFilter = ResourceCollectionFilter'
  { -- | Information about Amazon Web Services CloudFormation stacks. You can use
    -- up to 500 stacks to specify which Amazon Web Services resources in your
    -- account to analyze. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
    -- in the /Amazon Web Services CloudFormation User Guide/.
    cloudFormation :: Prelude.Maybe CloudFormationCollectionFilter,
    -- | The Amazon Web Services tags used to filter the resources in the
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
    tags :: Prelude.Maybe [TagCollectionFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCollectionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'resourceCollectionFilter_cloudFormation' - Information about Amazon Web Services CloudFormation stacks. You can use
-- up to 500 stacks to specify which Amazon Web Services resources in your
-- account to analyze. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /Amazon Web Services CloudFormation User Guide/.
--
-- 'tags', 'resourceCollectionFilter_tags' - The Amazon Web Services tags used to filter the resources in the
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
newResourceCollectionFilter ::
  ResourceCollectionFilter
newResourceCollectionFilter =
  ResourceCollectionFilter'
    { cloudFormation =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Information about Amazon Web Services CloudFormation stacks. You can use
-- up to 500 stacks to specify which Amazon Web Services resources in your
-- account to analyze. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacks.html Stacks>
-- in the /Amazon Web Services CloudFormation User Guide/.
resourceCollectionFilter_cloudFormation :: Lens.Lens' ResourceCollectionFilter (Prelude.Maybe CloudFormationCollectionFilter)
resourceCollectionFilter_cloudFormation = Lens.lens (\ResourceCollectionFilter' {cloudFormation} -> cloudFormation) (\s@ResourceCollectionFilter' {} a -> s {cloudFormation = a} :: ResourceCollectionFilter)

-- | The Amazon Web Services tags used to filter the resources in the
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
resourceCollectionFilter_tags :: Lens.Lens' ResourceCollectionFilter (Prelude.Maybe [TagCollectionFilter])
resourceCollectionFilter_tags = Lens.lens (\ResourceCollectionFilter' {tags} -> tags) (\s@ResourceCollectionFilter' {} a -> s {tags = a} :: ResourceCollectionFilter) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceCollectionFilter where
  parseJSON =
    Data.withObject
      "ResourceCollectionFilter"
      ( \x ->
          ResourceCollectionFilter'
            Prelude.<$> (x Data..:? "CloudFormation")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceCollectionFilter where
  hashWithSalt _salt ResourceCollectionFilter' {..} =
    _salt
      `Prelude.hashWithSalt` cloudFormation
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceCollectionFilter where
  rnf ResourceCollectionFilter' {..} =
    Prelude.rnf cloudFormation
      `Prelude.seq` Prelude.rnf tags
