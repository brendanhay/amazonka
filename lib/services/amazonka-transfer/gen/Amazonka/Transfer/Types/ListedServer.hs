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
-- Module      : Amazonka.Transfer.Types.ListedServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.Domain
import Amazonka.Transfer.Types.EndpointType
import Amazonka.Transfer.Types.IdentityProviderType
import Amazonka.Transfer.Types.State

-- | Returns properties of a file transfer protocol-enabled server that was
-- specified.
--
-- /See:/ 'newListedServer' smart constructor.
data ListedServer = ListedServer'
  { -- | Specifies the domain of the storage system that is used for file
    -- transfers.
    domain :: Prelude.Maybe Domain,
    -- | Specifies the type of VPC endpoint that your server is connected to. If
    -- your server is connected to a VPC endpoint, your server isn\'t
    -- accessible over the public internet.
    endpointType :: Prelude.Maybe EndpointType,
    -- | The mode of authentication for a server. The default value is
    -- @SERVICE_MANAGED@, which allows you to store and access user credentials
    -- within the Transfer Family service.
    --
    -- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
    -- in Directory Service for Microsoft Active Directory or Microsoft Active
    -- Directory in your on-premises environment or in Amazon Web Services
    -- using AD Connector. This option also requires you to provide a Directory
    -- ID by using the @IdentityProviderDetails@ parameter.
    --
    -- Use the @API_GATEWAY@ value to integrate with an identity provider of
    -- your choosing. The @API_GATEWAY@ setting requires you to provide an
    -- Amazon API Gateway endpoint URL to call for authentication by using the
    -- @IdentityProviderDetails@ parameter.
    --
    -- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
    -- identity provider. If you choose this value, you must specify the ARN
    -- for the Lambda function in the @Function@ parameter or the
    -- @IdentityProviderDetails@ data type.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
    -- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
    -- your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the unique system assigned identifier for the servers that
    -- were listed.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | The condition of the server that was described. A value of @ONLINE@
    -- indicates that the server can accept jobs and transfer files. A @State@
    -- value of @OFFLINE@ means that the server cannot perform file transfer
    -- operations.
    --
    -- The states of @STARTING@ and @STOPPING@ indicate that the server is in
    -- an intermediate state, either not fully able to respond, or not fully
    -- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
    -- error condition.
    state :: Prelude.Maybe State,
    -- | Specifies the number of users that are assigned to a server you
    -- specified with the @ServerId@.
    userCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the unique Amazon Resource Name (ARN) for a server to be
    -- listed.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'listedServer_domain' - Specifies the domain of the storage system that is used for file
-- transfers.
--
-- 'endpointType', 'listedServer_endpointType' - Specifies the type of VPC endpoint that your server is connected to. If
-- your server is connected to a VPC endpoint, your server isn\'t
-- accessible over the public internet.
--
-- 'identityProviderType', 'listedServer_identityProviderType' - The mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Directory Service for Microsoft Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connector. This option also requires you to provide a Directory
-- ID by using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an
-- Amazon API Gateway endpoint URL to call for authentication by using the
-- @IdentityProviderDetails@ parameter.
--
-- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
-- identity provider. If you choose this value, you must specify the ARN
-- for the Lambda function in the @Function@ parameter or the
-- @IdentityProviderDetails@ data type.
--
-- 'loggingRole', 'listedServer_loggingRole' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
--
-- 'serverId', 'listedServer_serverId' - Specifies the unique system assigned identifier for the servers that
-- were listed.
--
-- 'state', 'listedServer_state' - The condition of the server that was described. A value of @ONLINE@
-- indicates that the server can accept jobs and transfer files. A @State@
-- value of @OFFLINE@ means that the server cannot perform file transfer
-- operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
--
-- 'userCount', 'listedServer_userCount' - Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
--
-- 'arn', 'listedServer_arn' - Specifies the unique Amazon Resource Name (ARN) for a server to be
-- listed.
newListedServer ::
  -- | 'arn'
  Prelude.Text ->
  ListedServer
newListedServer pArn_ =
  ListedServer'
    { domain = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      loggingRole = Prelude.Nothing,
      serverId = Prelude.Nothing,
      state = Prelude.Nothing,
      userCount = Prelude.Nothing,
      arn = pArn_
    }

-- | Specifies the domain of the storage system that is used for file
-- transfers.
listedServer_domain :: Lens.Lens' ListedServer (Prelude.Maybe Domain)
listedServer_domain = Lens.lens (\ListedServer' {domain} -> domain) (\s@ListedServer' {} a -> s {domain = a} :: ListedServer)

-- | Specifies the type of VPC endpoint that your server is connected to. If
-- your server is connected to a VPC endpoint, your server isn\'t
-- accessible over the public internet.
listedServer_endpointType :: Lens.Lens' ListedServer (Prelude.Maybe EndpointType)
listedServer_endpointType = Lens.lens (\ListedServer' {endpointType} -> endpointType) (\s@ListedServer' {} a -> s {endpointType = a} :: ListedServer)

-- | The mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Directory Service for Microsoft Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connector. This option also requires you to provide a Directory
-- ID by using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an
-- Amazon API Gateway endpoint URL to call for authentication by using the
-- @IdentityProviderDetails@ parameter.
--
-- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
-- identity provider. If you choose this value, you must specify the ARN
-- for the Lambda function in the @Function@ parameter or the
-- @IdentityProviderDetails@ data type.
listedServer_identityProviderType :: Lens.Lens' ListedServer (Prelude.Maybe IdentityProviderType)
listedServer_identityProviderType = Lens.lens (\ListedServer' {identityProviderType} -> identityProviderType) (\s@ListedServer' {} a -> s {identityProviderType = a} :: ListedServer)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that allows a server to turn on Amazon CloudWatch logging for
-- Amazon S3 or Amazon EFSevents. When set, you can view user activity in
-- your CloudWatch logs.
listedServer_loggingRole :: Lens.Lens' ListedServer (Prelude.Maybe Prelude.Text)
listedServer_loggingRole = Lens.lens (\ListedServer' {loggingRole} -> loggingRole) (\s@ListedServer' {} a -> s {loggingRole = a} :: ListedServer)

-- | Specifies the unique system assigned identifier for the servers that
-- were listed.
listedServer_serverId :: Lens.Lens' ListedServer (Prelude.Maybe Prelude.Text)
listedServer_serverId = Lens.lens (\ListedServer' {serverId} -> serverId) (\s@ListedServer' {} a -> s {serverId = a} :: ListedServer)

-- | The condition of the server that was described. A value of @ONLINE@
-- indicates that the server can accept jobs and transfer files. A @State@
-- value of @OFFLINE@ means that the server cannot perform file transfer
-- operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
listedServer_state :: Lens.Lens' ListedServer (Prelude.Maybe State)
listedServer_state = Lens.lens (\ListedServer' {state} -> state) (\s@ListedServer' {} a -> s {state = a} :: ListedServer)

-- | Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
listedServer_userCount :: Lens.Lens' ListedServer (Prelude.Maybe Prelude.Int)
listedServer_userCount = Lens.lens (\ListedServer' {userCount} -> userCount) (\s@ListedServer' {} a -> s {userCount = a} :: ListedServer)

-- | Specifies the unique Amazon Resource Name (ARN) for a server to be
-- listed.
listedServer_arn :: Lens.Lens' ListedServer Prelude.Text
listedServer_arn = Lens.lens (\ListedServer' {arn} -> arn) (\s@ListedServer' {} a -> s {arn = a} :: ListedServer)

instance Data.FromJSON ListedServer where
  parseJSON =
    Data.withObject
      "ListedServer"
      ( \x ->
          ListedServer'
            Prelude.<$> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "EndpointType")
            Prelude.<*> (x Data..:? "IdentityProviderType")
            Prelude.<*> (x Data..:? "LoggingRole")
            Prelude.<*> (x Data..:? "ServerId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "UserCount")
            Prelude.<*> (x Data..: "Arn")
      )

instance Prelude.Hashable ListedServer where
  hashWithSalt _salt ListedServer' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` loggingRole
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` userCount
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListedServer where
  rnf ListedServer' {..} =
    Prelude.rnf domain `Prelude.seq`
      Prelude.rnf endpointType `Prelude.seq`
        Prelude.rnf identityProviderType `Prelude.seq`
          Prelude.rnf loggingRole `Prelude.seq`
            Prelude.rnf serverId `Prelude.seq`
              Prelude.rnf state `Prelude.seq`
                Prelude.rnf userCount `Prelude.seq`
                  Prelude.rnf arn
