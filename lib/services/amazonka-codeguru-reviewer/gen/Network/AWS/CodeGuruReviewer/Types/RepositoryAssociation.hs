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
-- Module      : Network.AWS.CodeGuruReviewer.Types.RepositoryAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruReviewer.Types.RepositoryAssociation where

import Network.AWS.CodeGuruReviewer.Types.KMSKeyDetails
import Network.AWS.CodeGuruReviewer.Types.ProviderType
import Network.AWS.CodeGuruReviewer.Types.RepositoryAssociationState
import Network.AWS.CodeGuruReviewer.Types.S3RepositoryDetails
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a repository association. The
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_DescribeRepositoryAssociation.html DescribeRepositoryAssociation>
-- operation returns a @RepositoryAssociation@ object.
--
-- /See:/ 'newRepositoryAssociation' smart constructor.
data RepositoryAssociation = RepositoryAssociation'
  { -- | The Amazon Resource Name (ARN) identifying the repository association.
    associationArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the repository association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The state of the repository association.
    --
    -- The valid repository association states are:
    --
    -- -   __Associated__: The repository association is complete.
    --
    -- -   __Associating__: CodeGuru Reviewer is:
    --
    --     -   Setting up pull request notifications. This is required for pull
    --         requests to trigger a CodeGuru Reviewer review.
    --
    --         If your repository @ProviderType@ is @GitHub@,
    --         @GitHub Enterprise Server@, or @Bitbucket@, CodeGuru Reviewer
    --         creates webhooks in your repository to trigger CodeGuru Reviewer
    --         reviews. If you delete these webhooks, reviews of code in your
    --         repository cannot be triggered.
    --
    --     -   Setting up source code access. This is required for CodeGuru
    --         Reviewer to securely clone code in your repository.
    --
    -- -   __Failed__: The repository failed to associate or disassociate.
    --
    -- -   __Disassociating__: CodeGuru Reviewer is removing the repository\'s
    --     pull request notifications and source code access.
    --
    -- -   __Disassociated__: CodeGuru Reviewer successfully disassociated the
    --     repository. You can create a new association with this repository if
    --     you want to review source code in it later. You can control access
    --     to code reviews created in an associated repository with tags after
    --     it has been disassociated. For more information, see
    --     <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/auth-and-access-control-using-tags.html Using tags to control access to associated repositories>
    --     in the /Amazon CodeGuru Reviewer User Guide/.
    state :: Prelude.Maybe RepositoryAssociationState,
    s3RepositoryDetails :: Prelude.Maybe S3RepositoryDetails,
    -- | The provider type of the repository association.
    providerType :: Prelude.Maybe ProviderType,
    -- | The owner of the repository. For an Amazon Web Services CodeCommit
    -- repository, this is the Amazon Web Services account ID of the account
    -- that owns the repository. For a GitHub, GitHub Enterprise Server, or
    -- Bitbucket repository, this is the username for the account that owns the
    -- repository. For an S3 repository, it can be the username or Amazon Web
    -- Services account ID.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    name :: Prelude.Maybe Prelude.Text,
    -- | A @KMSKeyDetails@ object that contains:
    --
    -- -   The encryption option for this repository association. It is either
    --     owned by Amazon Web Services Key Management Service (KMS)
    --     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
    --
    -- -   The ID of the Amazon Web Services KMS key that is associated with
    --     this respository association.
    kmsKeyDetails :: Prelude.Maybe KMSKeyDetails,
    -- | The Amazon Resource Name (ARN) of an Amazon Web Services CodeStar
    -- Connections connection. Its format is
    -- @arn:aws:codestar-connections:region-id:aws-account_id:connection\/connection-id@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/API_Connection.html Connection>
    -- in the /Amazon Web Services CodeStar Connections API Reference/.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A description of why the repository association is in the current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the repository
    -- association was created.
    createdTimeStamp :: Prelude.Maybe Core.POSIX,
    -- | The time, in milliseconds since the epoch, when the repository
    -- association was last updated.
    lastUpdatedTimeStamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationArn', 'repositoryAssociation_associationArn' - The Amazon Resource Name (ARN) identifying the repository association.
--
-- 'associationId', 'repositoryAssociation_associationId' - The ID of the repository association.
--
-- 'state', 'repositoryAssociation_state' - The state of the repository association.
--
-- The valid repository association states are:
--
-- -   __Associated__: The repository association is complete.
--
-- -   __Associating__: CodeGuru Reviewer is:
--
--     -   Setting up pull request notifications. This is required for pull
--         requests to trigger a CodeGuru Reviewer review.
--
--         If your repository @ProviderType@ is @GitHub@,
--         @GitHub Enterprise Server@, or @Bitbucket@, CodeGuru Reviewer
--         creates webhooks in your repository to trigger CodeGuru Reviewer
--         reviews. If you delete these webhooks, reviews of code in your
--         repository cannot be triggered.
--
--     -   Setting up source code access. This is required for CodeGuru
--         Reviewer to securely clone code in your repository.
--
-- -   __Failed__: The repository failed to associate or disassociate.
--
-- -   __Disassociating__: CodeGuru Reviewer is removing the repository\'s
--     pull request notifications and source code access.
--
-- -   __Disassociated__: CodeGuru Reviewer successfully disassociated the
--     repository. You can create a new association with this repository if
--     you want to review source code in it later. You can control access
--     to code reviews created in an associated repository with tags after
--     it has been disassociated. For more information, see
--     <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/auth-and-access-control-using-tags.html Using tags to control access to associated repositories>
--     in the /Amazon CodeGuru Reviewer User Guide/.
--
-- 's3RepositoryDetails', 'repositoryAssociation_s3RepositoryDetails' - Undocumented member.
--
-- 'providerType', 'repositoryAssociation_providerType' - The provider type of the repository association.
--
-- 'owner', 'repositoryAssociation_owner' - The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
--
-- 'name', 'repositoryAssociation_name' - The name of the repository.
--
-- 'kmsKeyDetails', 'repositoryAssociation_kmsKeyDetails' - A @KMSKeyDetails@ object that contains:
--
-- -   The encryption option for this repository association. It is either
--     owned by Amazon Web Services Key Management Service (KMS)
--     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- -   The ID of the Amazon Web Services KMS key that is associated with
--     this respository association.
--
-- 'connectionArn', 'repositoryAssociation_connectionArn' - The Amazon Resource Name (ARN) of an Amazon Web Services CodeStar
-- Connections connection. Its format is
-- @arn:aws:codestar-connections:region-id:aws-account_id:connection\/connection-id@.
-- For more information, see
-- <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/API_Connection.html Connection>
-- in the /Amazon Web Services CodeStar Connections API Reference/.
--
-- 'stateReason', 'repositoryAssociation_stateReason' - A description of why the repository association is in the current state.
--
-- 'createdTimeStamp', 'repositoryAssociation_createdTimeStamp' - The time, in milliseconds since the epoch, when the repository
-- association was created.
--
-- 'lastUpdatedTimeStamp', 'repositoryAssociation_lastUpdatedTimeStamp' - The time, in milliseconds since the epoch, when the repository
-- association was last updated.
newRepositoryAssociation ::
  RepositoryAssociation
newRepositoryAssociation =
  RepositoryAssociation'
    { associationArn =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      state = Prelude.Nothing,
      s3RepositoryDetails = Prelude.Nothing,
      providerType = Prelude.Nothing,
      owner = Prelude.Nothing,
      name = Prelude.Nothing,
      kmsKeyDetails = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      lastUpdatedTimeStamp = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) identifying the repository association.
repositoryAssociation_associationArn :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.Text)
repositoryAssociation_associationArn = Lens.lens (\RepositoryAssociation' {associationArn} -> associationArn) (\s@RepositoryAssociation' {} a -> s {associationArn = a} :: RepositoryAssociation)

-- | The ID of the repository association.
repositoryAssociation_associationId :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.Text)
repositoryAssociation_associationId = Lens.lens (\RepositoryAssociation' {associationId} -> associationId) (\s@RepositoryAssociation' {} a -> s {associationId = a} :: RepositoryAssociation)

-- | The state of the repository association.
--
-- The valid repository association states are:
--
-- -   __Associated__: The repository association is complete.
--
-- -   __Associating__: CodeGuru Reviewer is:
--
--     -   Setting up pull request notifications. This is required for pull
--         requests to trigger a CodeGuru Reviewer review.
--
--         If your repository @ProviderType@ is @GitHub@,
--         @GitHub Enterprise Server@, or @Bitbucket@, CodeGuru Reviewer
--         creates webhooks in your repository to trigger CodeGuru Reviewer
--         reviews. If you delete these webhooks, reviews of code in your
--         repository cannot be triggered.
--
--     -   Setting up source code access. This is required for CodeGuru
--         Reviewer to securely clone code in your repository.
--
-- -   __Failed__: The repository failed to associate or disassociate.
--
-- -   __Disassociating__: CodeGuru Reviewer is removing the repository\'s
--     pull request notifications and source code access.
--
-- -   __Disassociated__: CodeGuru Reviewer successfully disassociated the
--     repository. You can create a new association with this repository if
--     you want to review source code in it later. You can control access
--     to code reviews created in an associated repository with tags after
--     it has been disassociated. For more information, see
--     <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/auth-and-access-control-using-tags.html Using tags to control access to associated repositories>
--     in the /Amazon CodeGuru Reviewer User Guide/.
repositoryAssociation_state :: Lens.Lens' RepositoryAssociation (Prelude.Maybe RepositoryAssociationState)
repositoryAssociation_state = Lens.lens (\RepositoryAssociation' {state} -> state) (\s@RepositoryAssociation' {} a -> s {state = a} :: RepositoryAssociation)

-- | Undocumented member.
repositoryAssociation_s3RepositoryDetails :: Lens.Lens' RepositoryAssociation (Prelude.Maybe S3RepositoryDetails)
repositoryAssociation_s3RepositoryDetails = Lens.lens (\RepositoryAssociation' {s3RepositoryDetails} -> s3RepositoryDetails) (\s@RepositoryAssociation' {} a -> s {s3RepositoryDetails = a} :: RepositoryAssociation)

-- | The provider type of the repository association.
repositoryAssociation_providerType :: Lens.Lens' RepositoryAssociation (Prelude.Maybe ProviderType)
repositoryAssociation_providerType = Lens.lens (\RepositoryAssociation' {providerType} -> providerType) (\s@RepositoryAssociation' {} a -> s {providerType = a} :: RepositoryAssociation)

-- | The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
repositoryAssociation_owner :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.Text)
repositoryAssociation_owner = Lens.lens (\RepositoryAssociation' {owner} -> owner) (\s@RepositoryAssociation' {} a -> s {owner = a} :: RepositoryAssociation)

-- | The name of the repository.
repositoryAssociation_name :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.Text)
repositoryAssociation_name = Lens.lens (\RepositoryAssociation' {name} -> name) (\s@RepositoryAssociation' {} a -> s {name = a} :: RepositoryAssociation)

-- | A @KMSKeyDetails@ object that contains:
--
-- -   The encryption option for this repository association. It is either
--     owned by Amazon Web Services Key Management Service (KMS)
--     (@AWS_OWNED_CMK@) or customer managed (@CUSTOMER_MANAGED_CMK@).
--
-- -   The ID of the Amazon Web Services KMS key that is associated with
--     this respository association.
repositoryAssociation_kmsKeyDetails :: Lens.Lens' RepositoryAssociation (Prelude.Maybe KMSKeyDetails)
repositoryAssociation_kmsKeyDetails = Lens.lens (\RepositoryAssociation' {kmsKeyDetails} -> kmsKeyDetails) (\s@RepositoryAssociation' {} a -> s {kmsKeyDetails = a} :: RepositoryAssociation)

-- | The Amazon Resource Name (ARN) of an Amazon Web Services CodeStar
-- Connections connection. Its format is
-- @arn:aws:codestar-connections:region-id:aws-account_id:connection\/connection-id@.
-- For more information, see
-- <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/API_Connection.html Connection>
-- in the /Amazon Web Services CodeStar Connections API Reference/.
repositoryAssociation_connectionArn :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.Text)
repositoryAssociation_connectionArn = Lens.lens (\RepositoryAssociation' {connectionArn} -> connectionArn) (\s@RepositoryAssociation' {} a -> s {connectionArn = a} :: RepositoryAssociation)

-- | A description of why the repository association is in the current state.
repositoryAssociation_stateReason :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.Text)
repositoryAssociation_stateReason = Lens.lens (\RepositoryAssociation' {stateReason} -> stateReason) (\s@RepositoryAssociation' {} a -> s {stateReason = a} :: RepositoryAssociation)

-- | The time, in milliseconds since the epoch, when the repository
-- association was created.
repositoryAssociation_createdTimeStamp :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.UTCTime)
repositoryAssociation_createdTimeStamp = Lens.lens (\RepositoryAssociation' {createdTimeStamp} -> createdTimeStamp) (\s@RepositoryAssociation' {} a -> s {createdTimeStamp = a} :: RepositoryAssociation) Prelude.. Lens.mapping Core._Time

-- | The time, in milliseconds since the epoch, when the repository
-- association was last updated.
repositoryAssociation_lastUpdatedTimeStamp :: Lens.Lens' RepositoryAssociation (Prelude.Maybe Prelude.UTCTime)
repositoryAssociation_lastUpdatedTimeStamp = Lens.lens (\RepositoryAssociation' {lastUpdatedTimeStamp} -> lastUpdatedTimeStamp) (\s@RepositoryAssociation' {} a -> s {lastUpdatedTimeStamp = a} :: RepositoryAssociation) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON RepositoryAssociation where
  parseJSON =
    Core.withObject
      "RepositoryAssociation"
      ( \x ->
          RepositoryAssociation'
            Prelude.<$> (x Core..:? "AssociationArn")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "S3RepositoryDetails")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "KMSKeyDetails")
            Prelude.<*> (x Core..:? "ConnectionArn")
            Prelude.<*> (x Core..:? "StateReason")
            Prelude.<*> (x Core..:? "CreatedTimeStamp")
            Prelude.<*> (x Core..:? "LastUpdatedTimeStamp")
      )

instance Prelude.Hashable RepositoryAssociation

instance Prelude.NFData RepositoryAssociation
