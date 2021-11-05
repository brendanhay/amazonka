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
-- Module      : Amazonka.CodeGuruReviewer.Types.RepositoryAssociationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RepositoryAssociationSummary where

import Amazonka.CodeGuruReviewer.Types.ProviderType
import Amazonka.CodeGuruReviewer.Types.RepositoryAssociationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a repository association. The
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>
-- operation returns a list of @RepositoryAssociationSummary@ objects.
--
-- /See:/ 'newRepositoryAssociationSummary' smart constructor.
data RepositoryAssociationSummary = RepositoryAssociationSummary'
  { -- | The Amazon Resource Name (ARN) of the
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
    -- object. You can retrieve this ARN by calling
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>
    -- .
    associationArn :: Prelude.Maybe Prelude.Text,
    -- | The repository association ID.
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
    -- | The provider type of the repository association.
    providerType :: Prelude.Maybe ProviderType,
    -- | The owner of the repository. For an Amazon Web Services CodeCommit
    -- repository, this is the Amazon Web Services account ID of the account
    -- that owns the repository. For a GitHub, GitHub Enterprise Server, or
    -- Bitbucket repository, this is the username for the account that owns the
    -- repository. For an S3 repository, it can be the username or Amazon Web
    -- Services account ID.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository association.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Amazon Web Services CodeStar
    -- Connections connection. Its format is
    -- @arn:aws:codestar-connections:region-id:aws-account_id:connection\/connection-id@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/API_Connection.html Connection>
    -- in the /Amazon Web Services CodeStar Connections API Reference/.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, since the repository
    -- association was last updated.
    lastUpdatedTimeStamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationArn', 'repositoryAssociationSummary_associationArn' - The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object. You can retrieve this ARN by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>
-- .
--
-- 'associationId', 'repositoryAssociationSummary_associationId' - The repository association ID.
--
-- 'state', 'repositoryAssociationSummary_state' - The state of the repository association.
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
-- 'providerType', 'repositoryAssociationSummary_providerType' - The provider type of the repository association.
--
-- 'owner', 'repositoryAssociationSummary_owner' - The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
--
-- 'name', 'repositoryAssociationSummary_name' - The name of the repository association.
--
-- 'connectionArn', 'repositoryAssociationSummary_connectionArn' - The Amazon Resource Name (ARN) of an Amazon Web Services CodeStar
-- Connections connection. Its format is
-- @arn:aws:codestar-connections:region-id:aws-account_id:connection\/connection-id@.
-- For more information, see
-- <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/API_Connection.html Connection>
-- in the /Amazon Web Services CodeStar Connections API Reference/.
--
-- 'lastUpdatedTimeStamp', 'repositoryAssociationSummary_lastUpdatedTimeStamp' - The time, in milliseconds since the epoch, since the repository
-- association was last updated.
newRepositoryAssociationSummary ::
  RepositoryAssociationSummary
newRepositoryAssociationSummary =
  RepositoryAssociationSummary'
    { associationArn =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      state = Prelude.Nothing,
      providerType = Prelude.Nothing,
      owner = Prelude.Nothing,
      name = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      lastUpdatedTimeStamp = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>
-- object. You can retrieve this ARN by calling
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_ListRepositoryAssociations.html ListRepositoryAssociations>
-- .
repositoryAssociationSummary_associationArn :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe Prelude.Text)
repositoryAssociationSummary_associationArn = Lens.lens (\RepositoryAssociationSummary' {associationArn} -> associationArn) (\s@RepositoryAssociationSummary' {} a -> s {associationArn = a} :: RepositoryAssociationSummary)

-- | The repository association ID.
repositoryAssociationSummary_associationId :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe Prelude.Text)
repositoryAssociationSummary_associationId = Lens.lens (\RepositoryAssociationSummary' {associationId} -> associationId) (\s@RepositoryAssociationSummary' {} a -> s {associationId = a} :: RepositoryAssociationSummary)

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
repositoryAssociationSummary_state :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe RepositoryAssociationState)
repositoryAssociationSummary_state = Lens.lens (\RepositoryAssociationSummary' {state} -> state) (\s@RepositoryAssociationSummary' {} a -> s {state = a} :: RepositoryAssociationSummary)

-- | The provider type of the repository association.
repositoryAssociationSummary_providerType :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe ProviderType)
repositoryAssociationSummary_providerType = Lens.lens (\RepositoryAssociationSummary' {providerType} -> providerType) (\s@RepositoryAssociationSummary' {} a -> s {providerType = a} :: RepositoryAssociationSummary)

-- | The owner of the repository. For an Amazon Web Services CodeCommit
-- repository, this is the Amazon Web Services account ID of the account
-- that owns the repository. For a GitHub, GitHub Enterprise Server, or
-- Bitbucket repository, this is the username for the account that owns the
-- repository. For an S3 repository, it can be the username or Amazon Web
-- Services account ID.
repositoryAssociationSummary_owner :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe Prelude.Text)
repositoryAssociationSummary_owner = Lens.lens (\RepositoryAssociationSummary' {owner} -> owner) (\s@RepositoryAssociationSummary' {} a -> s {owner = a} :: RepositoryAssociationSummary)

-- | The name of the repository association.
repositoryAssociationSummary_name :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe Prelude.Text)
repositoryAssociationSummary_name = Lens.lens (\RepositoryAssociationSummary' {name} -> name) (\s@RepositoryAssociationSummary' {} a -> s {name = a} :: RepositoryAssociationSummary)

-- | The Amazon Resource Name (ARN) of an Amazon Web Services CodeStar
-- Connections connection. Its format is
-- @arn:aws:codestar-connections:region-id:aws-account_id:connection\/connection-id@.
-- For more information, see
-- <https://docs.aws.amazon.com/codestar-connections/latest/APIReference/API_Connection.html Connection>
-- in the /Amazon Web Services CodeStar Connections API Reference/.
repositoryAssociationSummary_connectionArn :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe Prelude.Text)
repositoryAssociationSummary_connectionArn = Lens.lens (\RepositoryAssociationSummary' {connectionArn} -> connectionArn) (\s@RepositoryAssociationSummary' {} a -> s {connectionArn = a} :: RepositoryAssociationSummary)

-- | The time, in milliseconds since the epoch, since the repository
-- association was last updated.
repositoryAssociationSummary_lastUpdatedTimeStamp :: Lens.Lens' RepositoryAssociationSummary (Prelude.Maybe Prelude.UTCTime)
repositoryAssociationSummary_lastUpdatedTimeStamp = Lens.lens (\RepositoryAssociationSummary' {lastUpdatedTimeStamp} -> lastUpdatedTimeStamp) (\s@RepositoryAssociationSummary' {} a -> s {lastUpdatedTimeStamp = a} :: RepositoryAssociationSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON RepositoryAssociationSummary where
  parseJSON =
    Core.withObject
      "RepositoryAssociationSummary"
      ( \x ->
          RepositoryAssociationSummary'
            Prelude.<$> (x Core..:? "AssociationArn")
            Prelude.<*> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ProviderType")
            Prelude.<*> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ConnectionArn")
            Prelude.<*> (x Core..:? "LastUpdatedTimeStamp")
      )

instance
  Prelude.Hashable
    RepositoryAssociationSummary

instance Prelude.NFData RepositoryAssociationSummary
