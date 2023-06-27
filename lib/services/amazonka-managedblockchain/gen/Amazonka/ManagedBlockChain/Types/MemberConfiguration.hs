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
-- Module      : Amazonka.ManagedBlockChain.Types.MemberConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.MemberConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types.MemberFrameworkConfiguration
import Amazonka.ManagedBlockChain.Types.MemberLogPublishingConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties of the member.
--
-- Applies only to Hyperledger Fabric.
--
-- /See:/ 'newMemberConfiguration' smart constructor.
data MemberConfiguration = MemberConfiguration'
  { -- | An optional description of the member.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the customer managed key in Key
    -- Management Service (KMS) to use for encryption at rest in the member.
    -- This parameter is inherited by any nodes that this member creates. For
    -- more information, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/managed-blockchain-encryption-at-rest.html Encryption at Rest>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    --
    -- Use one of the following options to specify this parameter:
    --
    -- -   __Undefined or empty string__ - By default, use an KMS key that is
    --     owned and managed by Amazon Web Services on your behalf.
    --
    -- -   __A valid symmetric customer managed KMS key__ - Use the specified
    --     KMS key in your account that you create, own, and manage.
    --
    --     Amazon Managed Blockchain doesn\'t support asymmetric keys. For more
    --     information, see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
    --     in the /Key Management Service Developer Guide/.
    --
    --     The following is an example of a KMS key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties for logging events associated with a member of
    -- a Managed Blockchain network.
    logPublishingConfiguration :: Prelude.Maybe MemberLogPublishingConfiguration,
    -- | Tags assigned to the member. Tags consist of a key and optional value.
    --
    -- When specifying tags during creation, you can specify multiple key-value
    -- pairs in a single request, with an overall maximum of 50 tags added to
    -- each resource.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
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
-- 'description', 'memberConfiguration_description' - An optional description of the member.
--
-- 'kmsKeyArn', 'memberConfiguration_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed key in Key
-- Management Service (KMS) to use for encryption at rest in the member.
-- This parameter is inherited by any nodes that this member creates. For
-- more information, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/managed-blockchain-encryption-at-rest.html Encryption at Rest>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- Use one of the following options to specify this parameter:
--
-- -   __Undefined or empty string__ - By default, use an KMS key that is
--     owned and managed by Amazon Web Services on your behalf.
--
-- -   __A valid symmetric customer managed KMS key__ - Use the specified
--     KMS key in your account that you create, own, and manage.
--
--     Amazon Managed Blockchain doesn\'t support asymmetric keys. For more
--     information, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
--     in the /Key Management Service Developer Guide/.
--
--     The following is an example of a KMS key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- 'logPublishingConfiguration', 'memberConfiguration_logPublishingConfiguration' - Configuration properties for logging events associated with a member of
-- a Managed Blockchain network.
--
-- 'tags', 'memberConfiguration_tags' - Tags assigned to the member. Tags consist of a key and optional value.
--
-- When specifying tags during creation, you can specify multiple key-value
-- pairs in a single request, with an overall maximum of 50 tags added to
-- each resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
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
      { description = Prelude.Nothing,
        kmsKeyArn = Prelude.Nothing,
        logPublishingConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        frameworkConfiguration = pFrameworkConfiguration_
      }

-- | An optional description of the member.
memberConfiguration_description :: Lens.Lens' MemberConfiguration (Prelude.Maybe Prelude.Text)
memberConfiguration_description = Lens.lens (\MemberConfiguration' {description} -> description) (\s@MemberConfiguration' {} a -> s {description = a} :: MemberConfiguration)

-- | The Amazon Resource Name (ARN) of the customer managed key in Key
-- Management Service (KMS) to use for encryption at rest in the member.
-- This parameter is inherited by any nodes that this member creates. For
-- more information, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/managed-blockchain-encryption-at-rest.html Encryption at Rest>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
--
-- Use one of the following options to specify this parameter:
--
-- -   __Undefined or empty string__ - By default, use an KMS key that is
--     owned and managed by Amazon Web Services on your behalf.
--
-- -   __A valid symmetric customer managed KMS key__ - Use the specified
--     KMS key in your account that you create, own, and manage.
--
--     Amazon Managed Blockchain doesn\'t support asymmetric keys. For more
--     information, see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
--     in the /Key Management Service Developer Guide/.
--
--     The following is an example of a KMS key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
memberConfiguration_kmsKeyArn :: Lens.Lens' MemberConfiguration (Prelude.Maybe Prelude.Text)
memberConfiguration_kmsKeyArn = Lens.lens (\MemberConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@MemberConfiguration' {} a -> s {kmsKeyArn = a} :: MemberConfiguration)

-- | Configuration properties for logging events associated with a member of
-- a Managed Blockchain network.
memberConfiguration_logPublishingConfiguration :: Lens.Lens' MemberConfiguration (Prelude.Maybe MemberLogPublishingConfiguration)
memberConfiguration_logPublishingConfiguration = Lens.lens (\MemberConfiguration' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@MemberConfiguration' {} a -> s {logPublishingConfiguration = a} :: MemberConfiguration)

-- | Tags assigned to the member. Tags consist of a key and optional value.
--
-- When specifying tags during creation, you can specify multiple key-value
-- pairs in a single request, with an overall maximum of 50 tags added to
-- each resource.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
memberConfiguration_tags :: Lens.Lens' MemberConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
memberConfiguration_tags = Lens.lens (\MemberConfiguration' {tags} -> tags) (\s@MemberConfiguration' {} a -> s {tags = a} :: MemberConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the member.
memberConfiguration_name :: Lens.Lens' MemberConfiguration Prelude.Text
memberConfiguration_name = Lens.lens (\MemberConfiguration' {name} -> name) (\s@MemberConfiguration' {} a -> s {name = a} :: MemberConfiguration)

-- | Configuration properties of the blockchain framework relevant to the
-- member.
memberConfiguration_frameworkConfiguration :: Lens.Lens' MemberConfiguration MemberFrameworkConfiguration
memberConfiguration_frameworkConfiguration = Lens.lens (\MemberConfiguration' {frameworkConfiguration} -> frameworkConfiguration) (\s@MemberConfiguration' {} a -> s {frameworkConfiguration = a} :: MemberConfiguration)

instance Prelude.Hashable MemberConfiguration where
  hashWithSalt _salt MemberConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` logPublishingConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` frameworkConfiguration

instance Prelude.NFData MemberConfiguration where
  rnf MemberConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf logPublishingConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf frameworkConfiguration

instance Data.ToJSON MemberConfiguration where
  toJSON MemberConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("LogPublishingConfiguration" Data..=)
              Prelude.<$> logPublishingConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "FrameworkConfiguration"
                  Data..= frameworkConfiguration
              )
          ]
      )
