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
-- Module      : Amazonka.DevOpsGuru.Types.ResourceCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ResourceCollection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.CloudFormationCollection
import Amazonka.DevOpsGuru.Types.TagCollection
import qualified Amazonka.Prelude as Prelude

-- | A collection of Amazon Web Services resources supported by DevOps Guru.
-- The two types of Amazon Web Services resource collections supported are
-- Amazon Web Services CloudFormation stacks and Amazon Web Services
-- resources that contain the same Amazon Web Services tag. DevOps Guru can
-- be configured to analyze the Amazon Web Services resources that are
-- defined in the stacks or that are tagged using the same tag /key/. You
-- can specify up to 500 Amazon Web Services CloudFormation stacks.
--
-- /See:/ 'newResourceCollection' smart constructor.
data ResourceCollection = ResourceCollection'
  { -- | An array of the names of Amazon Web Services CloudFormation stacks. The
    -- stacks define Amazon Web Services resources that DevOps Guru analyzes.
    -- You can specify up to 500 Amazon Web Services CloudFormation stacks.
    cloudFormation :: Prelude.Maybe CloudFormationCollection,
    -- | The Amazon Web Services tags that are used by resources in the resource
    -- collection.
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
    tags :: Prelude.Maybe [TagCollection]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormation', 'resourceCollection_cloudFormation' - An array of the names of Amazon Web Services CloudFormation stacks. The
-- stacks define Amazon Web Services resources that DevOps Guru analyzes.
-- You can specify up to 500 Amazon Web Services CloudFormation stacks.
--
-- 'tags', 'resourceCollection_tags' - The Amazon Web Services tags that are used by resources in the resource
-- collection.
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
newResourceCollection ::
  ResourceCollection
newResourceCollection =
  ResourceCollection'
    { cloudFormation =
        Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | An array of the names of Amazon Web Services CloudFormation stacks. The
-- stacks define Amazon Web Services resources that DevOps Guru analyzes.
-- You can specify up to 500 Amazon Web Services CloudFormation stacks.
resourceCollection_cloudFormation :: Lens.Lens' ResourceCollection (Prelude.Maybe CloudFormationCollection)
resourceCollection_cloudFormation = Lens.lens (\ResourceCollection' {cloudFormation} -> cloudFormation) (\s@ResourceCollection' {} a -> s {cloudFormation = a} :: ResourceCollection)

-- | The Amazon Web Services tags that are used by resources in the resource
-- collection.
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
resourceCollection_tags :: Lens.Lens' ResourceCollection (Prelude.Maybe [TagCollection])
resourceCollection_tags = Lens.lens (\ResourceCollection' {tags} -> tags) (\s@ResourceCollection' {} a -> s {tags = a} :: ResourceCollection) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceCollection where
  parseJSON =
    Data.withObject
      "ResourceCollection"
      ( \x ->
          ResourceCollection'
            Prelude.<$> (x Data..:? "CloudFormation")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceCollection where
  hashWithSalt _salt ResourceCollection' {..} =
    _salt
      `Prelude.hashWithSalt` cloudFormation
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ResourceCollection where
  rnf ResourceCollection' {..} =
    Prelude.rnf cloudFormation
      `Prelude.seq` Prelude.rnf tags

instance Data.ToJSON ResourceCollection where
  toJSON ResourceCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudFormation" Data..=)
              Prelude.<$> cloudFormation,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )
