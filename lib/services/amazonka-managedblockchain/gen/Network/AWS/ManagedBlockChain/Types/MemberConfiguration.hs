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
-- Module      : Network.AWS.ManagedBlockChain.Types.MemberConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ManagedBlockChain.Types.MemberConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ManagedBlockChain.Types.MemberFrameworkConfiguration
import Network.AWS.ManagedBlockChain.Types.MemberLogPublishingConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | Configuration properties of the member.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newMemberConfiguration' smart constructor.
data MemberConfiguration = MemberConfiguration'
  { -- | The Amazon Resource Name (ARN) of the customer managed key in AWS Key
    -- Management Service (AWS KMS) to use for encryption at rest in the
    -- member. This parameter is inherited by any nodes that this member
    -- creates.
    --
    -- Use one of the following options to specify this parameter:
    --
    -- -   __Undefined or empty string__ - The member uses an AWS owned KMS key
    --     for encryption by default.
    --
    -- -   __A valid symmetric customer managed KMS key__ - The member uses the
    --     specified key for encryption.
    --
    --     Amazon Managed Blockchain doesn\'t support asymmetric keys. For more
    --     information, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
    --     in the /AWS Key Management Service Developer Guide/.
    --
    --     The following is an example of a KMS key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties for logging events associated with a member of
    -- a Managed Blockchain network.
    logPublishingConfiguration :: Prelude.Maybe MemberLogPublishingConfiguration,
    -- | An optional description of the member.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to the member. Tags consist of a key and optional value.
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    --
    -- When specifying tags during creation, you can specify multiple key-value
    -- pairs in a single request, with an overall maximum of 50 tags added to
    -- each resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the member.
    name :: Prelude.Text,
    -- | Configuration properties of the blockchain framework relevant to the
    -- member.
    frameworkConfiguration :: MemberFrameworkConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'memberConfiguration_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed key in AWS Key
-- Management Service (AWS KMS) to use for encryption at rest in the
-- member. This parameter is inherited by any nodes that this member
-- creates.
--
-- Use one of the following options to specify this parameter:
--
-- -   __Undefined or empty string__ - The member uses an AWS owned KMS key
--     for encryption by default.
--
-- -   __A valid symmetric customer managed KMS key__ - The member uses the
--     specified key for encryption.
--
--     Amazon Managed Blockchain doesn\'t support asymmetric keys. For more
--     information, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
--     in the /AWS Key Management Service Developer Guide/.
--
--     The following is an example of a KMS key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- 'logPublishingConfiguration', 'memberConfiguration_logPublishingConfiguration' - Configuration properties for logging events associated with a member of
-- a Managed Blockchain network.
--
-- 'description', 'memberConfiguration_description' - An optional description of the member.
--
-- 'tags', 'memberConfiguration_tags' - Tags assigned to the member. Tags consist of a key and optional value.
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- When specifying tags during creation, you can specify multiple key-value
-- pairs in a single request, with an overall maximum of 50 tags added to
-- each resource.
--
-- 'name', 'memberConfiguration_name' - The name of the member.
--
-- 'frameworkConfiguration', 'memberConfiguration_frameworkConfiguration' - Configuration properties of the blockchain framework relevant to the
-- member.
newMemberConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'frameworkConfiguration'
  MemberFrameworkConfiguration ->
  MemberConfiguration
newMemberConfiguration
  pName_
  pFrameworkConfiguration_ =
    MemberConfiguration'
      { kmsKeyArn = Prelude.Nothing,
        logPublishingConfiguration = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        frameworkConfiguration = pFrameworkConfiguration_
      }

-- | The Amazon Resource Name (ARN) of the customer managed key in AWS Key
-- Management Service (AWS KMS) to use for encryption at rest in the
-- member. This parameter is inherited by any nodes that this member
-- creates.
--
-- Use one of the following options to specify this parameter:
--
-- -   __Undefined or empty string__ - The member uses an AWS owned KMS key
--     for encryption by default.
--
-- -   __A valid symmetric customer managed KMS key__ - The member uses the
--     specified key for encryption.
--
--     Amazon Managed Blockchain doesn\'t support asymmetric keys. For more
--     information, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
--     in the /AWS Key Management Service Developer Guide/.
--
--     The following is an example of a KMS key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
memberConfiguration_kmsKeyArn :: Lens.Lens' MemberConfiguration (Prelude.Maybe Prelude.Text)
memberConfiguration_kmsKeyArn = Lens.lens (\MemberConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@MemberConfiguration' {} a -> s {kmsKeyArn = a} :: MemberConfiguration)

-- | Configuration properties for logging events associated with a member of
-- a Managed Blockchain network.
memberConfiguration_logPublishingConfiguration :: Lens.Lens' MemberConfiguration (Prelude.Maybe MemberLogPublishingConfiguration)
memberConfiguration_logPublishingConfiguration = Lens.lens (\MemberConfiguration' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@MemberConfiguration' {} a -> s {logPublishingConfiguration = a} :: MemberConfiguration)

-- | An optional description of the member.
memberConfiguration_description :: Lens.Lens' MemberConfiguration (Prelude.Maybe Prelude.Text)
memberConfiguration_description = Lens.lens (\MemberConfiguration' {description} -> description) (\s@MemberConfiguration' {} a -> s {description = a} :: MemberConfiguration)

-- | Tags assigned to the member. Tags consist of a key and optional value.
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- When specifying tags during creation, you can specify multiple key-value
-- pairs in a single request, with an overall maximum of 50 tags added to
-- each resource.
memberConfiguration_tags :: Lens.Lens' MemberConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
memberConfiguration_tags = Lens.lens (\MemberConfiguration' {tags} -> tags) (\s@MemberConfiguration' {} a -> s {tags = a} :: MemberConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the member.
memberConfiguration_name :: Lens.Lens' MemberConfiguration Prelude.Text
memberConfiguration_name = Lens.lens (\MemberConfiguration' {name} -> name) (\s@MemberConfiguration' {} a -> s {name = a} :: MemberConfiguration)

-- | Configuration properties of the blockchain framework relevant to the
-- member.
memberConfiguration_frameworkConfiguration :: Lens.Lens' MemberConfiguration MemberFrameworkConfiguration
memberConfiguration_frameworkConfiguration = Lens.lens (\MemberConfiguration' {frameworkConfiguration} -> frameworkConfiguration) (\s@MemberConfiguration' {} a -> s {frameworkConfiguration = a} :: MemberConfiguration)

instance Prelude.Hashable MemberConfiguration

instance Prelude.NFData MemberConfiguration

instance Core.ToJSON MemberConfiguration where
  toJSON MemberConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            ("LogPublishingConfiguration" Core..=)
              Prelude.<$> logPublishingConfiguration,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ( "FrameworkConfiguration"
                  Core..= frameworkConfiguration
              )
          ]
      )
