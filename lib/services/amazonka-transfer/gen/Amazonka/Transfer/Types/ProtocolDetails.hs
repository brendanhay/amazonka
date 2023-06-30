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
-- Module      : Amazonka.Transfer.Types.ProtocolDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ProtocolDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.As2Transport
import Amazonka.Transfer.Types.SetStatOption
import Amazonka.Transfer.Types.TlsSessionResumptionMode

-- | The protocol settings that are configured for your server.
--
-- /See:/ 'newProtocolDetails' smart constructor.
data ProtocolDetails = ProtocolDetails'
  { -- | Indicates the transport method for the AS2 messages. Currently, only
    -- HTTP is supported.
    as2Transports :: Prelude.Maybe (Prelude.NonEmpty As2Transport),
    -- | Indicates passive mode, for FTP and FTPS protocols. Enter a single IPv4
    -- address, such as the public IP address of a firewall, router, or load
    -- balancer. For example:
    --
    -- @aws transfer update-server --protocol-details PassiveIp=0.0.0.0@
    --
    -- Replace @0.0.0.0@ in the example above with the actual IP address you
    -- want to use.
    --
    -- If you change the @PassiveIp@ value, you must stop and then restart your
    -- Transfer Family server for the change to take effect. For details on
    -- using passive mode (PASV) in a NAT environment, see
    -- <http://aws.amazon.com/blogs/storage/configuring-your-ftps-server-behind-a-firewall-or-nat-with-aws-transfer-family/ Configuring your FTPS server behind a firewall or NAT with Transfer Family>.
    --
    -- /Special values/
    --
    -- The @AUTO@ and @0.0.0.0@ are special values for the @PassiveIp@
    -- parameter. The value @PassiveIp=AUTO@ is assigned by default to FTP and
    -- FTPS type servers. In this case, the server automatically responds with
    -- one of the endpoint IPs within the PASV response. @PassiveIp=0.0.0.0@
    -- has a more unique application for its usage. For example, if you have a
    -- High Availability (HA) Network Load Balancer (NLB) environment, where
    -- you have 3 subnets, you can only specify a single IP address using the
    -- @PassiveIp@ parameter. This reduces the effectiveness of having High
    -- Availability. In this case, you can specify @PassiveIp=0.0.0.0@. This
    -- tells the client to use the same IP address as the Control connection
    -- and utilize all AZs for their connections. Note, however, that not all
    -- FTP clients support the @PassiveIp=0.0.0.0@ response. FileZilla and
    -- WinSCP do support it. If you are using other clients, check to see if
    -- your client supports the @PassiveIp=0.0.0.0@ response.
    passiveIp :: Prelude.Maybe Prelude.Text,
    -- | Use the @SetStatOption@ to ignore the error that is generated when the
    -- client attempts to use @SETSTAT@ on a file you are uploading to an S3
    -- bucket.
    --
    -- Some SFTP file transfer clients can attempt to change the attributes of
    -- remote files, including timestamp and permissions, using commands, such
    -- as @SETSTAT@ when uploading the file. However, these commands are not
    -- compatible with object storage systems, such as Amazon S3. Due to this
    -- incompatibility, file uploads from these clients can result in errors
    -- even when the file is otherwise successfully uploaded.
    --
    -- Set the value to @ENABLE_NO_OP@ to have the Transfer Family server
    -- ignore the @SETSTAT@ command, and upload files without needing to make
    -- any changes to your SFTP client. While the @SetStatOption@
    -- @ENABLE_NO_OP@ setting ignores the error, it does generate a log entry
    -- in Amazon CloudWatch Logs, so you can determine when the client is
    -- making a @SETSTAT@ call.
    --
    -- If you want to preserve the original timestamp for your file, and modify
    -- other file attributes using @SETSTAT@, you can use Amazon EFS as backend
    -- storage with Transfer Family.
    setStatOption :: Prelude.Maybe SetStatOption,
    -- | A property used with Transfer Family servers that use the FTPS protocol.
    -- TLS Session Resumption provides a mechanism to resume or share a
    -- negotiated secret key between the control and data connection for an
    -- FTPS session. @TlsSessionResumptionMode@ determines whether or not the
    -- server resumes recent, negotiated sessions through a unique session ID.
    -- This property is available during @CreateServer@ and @UpdateServer@
    -- calls. If a @TlsSessionResumptionMode@ value is not specified during
    -- @CreateServer@, it is set to @ENFORCED@ by default.
    --
    -- -   @DISABLED@: the server does not process TLS session resumption
    --     client requests and creates a new TLS session for each request.
    --
    -- -   @ENABLED@: the server processes and accepts clients that are
    --     performing TLS session resumption. The server doesn\'t reject client
    --     data connections that do not perform the TLS session resumption
    --     client processing.
    --
    -- -   @ENFORCED@: the server processes and accepts clients that are
    --     performing TLS session resumption. The server rejects client data
    --     connections that do not perform the TLS session resumption client
    --     processing. Before you set the value to @ENFORCED@, test your
    --     clients.
    --
    --     Not all FTPS clients perform TLS session resumption. So, if you
    --     choose to enforce TLS session resumption, you prevent any
    --     connections from FTPS clients that don\'t perform the protocol
    --     negotiation. To determine whether or not you can use the @ENFORCED@
    --     value, you need to test your clients.
    tlsSessionResumptionMode :: Prelude.Maybe TlsSessionResumptionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtocolDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'as2Transports', 'protocolDetails_as2Transports' - Indicates the transport method for the AS2 messages. Currently, only
-- HTTP is supported.
--
-- 'passiveIp', 'protocolDetails_passiveIp' - Indicates passive mode, for FTP and FTPS protocols. Enter a single IPv4
-- address, such as the public IP address of a firewall, router, or load
-- balancer. For example:
--
-- @aws transfer update-server --protocol-details PassiveIp=0.0.0.0@
--
-- Replace @0.0.0.0@ in the example above with the actual IP address you
-- want to use.
--
-- If you change the @PassiveIp@ value, you must stop and then restart your
-- Transfer Family server for the change to take effect. For details on
-- using passive mode (PASV) in a NAT environment, see
-- <http://aws.amazon.com/blogs/storage/configuring-your-ftps-server-behind-a-firewall-or-nat-with-aws-transfer-family/ Configuring your FTPS server behind a firewall or NAT with Transfer Family>.
--
-- /Special values/
--
-- The @AUTO@ and @0.0.0.0@ are special values for the @PassiveIp@
-- parameter. The value @PassiveIp=AUTO@ is assigned by default to FTP and
-- FTPS type servers. In this case, the server automatically responds with
-- one of the endpoint IPs within the PASV response. @PassiveIp=0.0.0.0@
-- has a more unique application for its usage. For example, if you have a
-- High Availability (HA) Network Load Balancer (NLB) environment, where
-- you have 3 subnets, you can only specify a single IP address using the
-- @PassiveIp@ parameter. This reduces the effectiveness of having High
-- Availability. In this case, you can specify @PassiveIp=0.0.0.0@. This
-- tells the client to use the same IP address as the Control connection
-- and utilize all AZs for their connections. Note, however, that not all
-- FTP clients support the @PassiveIp=0.0.0.0@ response. FileZilla and
-- WinSCP do support it. If you are using other clients, check to see if
-- your client supports the @PassiveIp=0.0.0.0@ response.
--
-- 'setStatOption', 'protocolDetails_setStatOption' - Use the @SetStatOption@ to ignore the error that is generated when the
-- client attempts to use @SETSTAT@ on a file you are uploading to an S3
-- bucket.
--
-- Some SFTP file transfer clients can attempt to change the attributes of
-- remote files, including timestamp and permissions, using commands, such
-- as @SETSTAT@ when uploading the file. However, these commands are not
-- compatible with object storage systems, such as Amazon S3. Due to this
-- incompatibility, file uploads from these clients can result in errors
-- even when the file is otherwise successfully uploaded.
--
-- Set the value to @ENABLE_NO_OP@ to have the Transfer Family server
-- ignore the @SETSTAT@ command, and upload files without needing to make
-- any changes to your SFTP client. While the @SetStatOption@
-- @ENABLE_NO_OP@ setting ignores the error, it does generate a log entry
-- in Amazon CloudWatch Logs, so you can determine when the client is
-- making a @SETSTAT@ call.
--
-- If you want to preserve the original timestamp for your file, and modify
-- other file attributes using @SETSTAT@, you can use Amazon EFS as backend
-- storage with Transfer Family.
--
-- 'tlsSessionResumptionMode', 'protocolDetails_tlsSessionResumptionMode' - A property used with Transfer Family servers that use the FTPS protocol.
-- TLS Session Resumption provides a mechanism to resume or share a
-- negotiated secret key between the control and data connection for an
-- FTPS session. @TlsSessionResumptionMode@ determines whether or not the
-- server resumes recent, negotiated sessions through a unique session ID.
-- This property is available during @CreateServer@ and @UpdateServer@
-- calls. If a @TlsSessionResumptionMode@ value is not specified during
-- @CreateServer@, it is set to @ENFORCED@ by default.
--
-- -   @DISABLED@: the server does not process TLS session resumption
--     client requests and creates a new TLS session for each request.
--
-- -   @ENABLED@: the server processes and accepts clients that are
--     performing TLS session resumption. The server doesn\'t reject client
--     data connections that do not perform the TLS session resumption
--     client processing.
--
-- -   @ENFORCED@: the server processes and accepts clients that are
--     performing TLS session resumption. The server rejects client data
--     connections that do not perform the TLS session resumption client
--     processing. Before you set the value to @ENFORCED@, test your
--     clients.
--
--     Not all FTPS clients perform TLS session resumption. So, if you
--     choose to enforce TLS session resumption, you prevent any
--     connections from FTPS clients that don\'t perform the protocol
--     negotiation. To determine whether or not you can use the @ENFORCED@
--     value, you need to test your clients.
newProtocolDetails ::
  ProtocolDetails
newProtocolDetails =
  ProtocolDetails'
    { as2Transports = Prelude.Nothing,
      passiveIp = Prelude.Nothing,
      setStatOption = Prelude.Nothing,
      tlsSessionResumptionMode = Prelude.Nothing
    }

-- | Indicates the transport method for the AS2 messages. Currently, only
-- HTTP is supported.
protocolDetails_as2Transports :: Lens.Lens' ProtocolDetails (Prelude.Maybe (Prelude.NonEmpty As2Transport))
protocolDetails_as2Transports = Lens.lens (\ProtocolDetails' {as2Transports} -> as2Transports) (\s@ProtocolDetails' {} a -> s {as2Transports = a} :: ProtocolDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates passive mode, for FTP and FTPS protocols. Enter a single IPv4
-- address, such as the public IP address of a firewall, router, or load
-- balancer. For example:
--
-- @aws transfer update-server --protocol-details PassiveIp=0.0.0.0@
--
-- Replace @0.0.0.0@ in the example above with the actual IP address you
-- want to use.
--
-- If you change the @PassiveIp@ value, you must stop and then restart your
-- Transfer Family server for the change to take effect. For details on
-- using passive mode (PASV) in a NAT environment, see
-- <http://aws.amazon.com/blogs/storage/configuring-your-ftps-server-behind-a-firewall-or-nat-with-aws-transfer-family/ Configuring your FTPS server behind a firewall or NAT with Transfer Family>.
--
-- /Special values/
--
-- The @AUTO@ and @0.0.0.0@ are special values for the @PassiveIp@
-- parameter. The value @PassiveIp=AUTO@ is assigned by default to FTP and
-- FTPS type servers. In this case, the server automatically responds with
-- one of the endpoint IPs within the PASV response. @PassiveIp=0.0.0.0@
-- has a more unique application for its usage. For example, if you have a
-- High Availability (HA) Network Load Balancer (NLB) environment, where
-- you have 3 subnets, you can only specify a single IP address using the
-- @PassiveIp@ parameter. This reduces the effectiveness of having High
-- Availability. In this case, you can specify @PassiveIp=0.0.0.0@. This
-- tells the client to use the same IP address as the Control connection
-- and utilize all AZs for their connections. Note, however, that not all
-- FTP clients support the @PassiveIp=0.0.0.0@ response. FileZilla and
-- WinSCP do support it. If you are using other clients, check to see if
-- your client supports the @PassiveIp=0.0.0.0@ response.
protocolDetails_passiveIp :: Lens.Lens' ProtocolDetails (Prelude.Maybe Prelude.Text)
protocolDetails_passiveIp = Lens.lens (\ProtocolDetails' {passiveIp} -> passiveIp) (\s@ProtocolDetails' {} a -> s {passiveIp = a} :: ProtocolDetails)

-- | Use the @SetStatOption@ to ignore the error that is generated when the
-- client attempts to use @SETSTAT@ on a file you are uploading to an S3
-- bucket.
--
-- Some SFTP file transfer clients can attempt to change the attributes of
-- remote files, including timestamp and permissions, using commands, such
-- as @SETSTAT@ when uploading the file. However, these commands are not
-- compatible with object storage systems, such as Amazon S3. Due to this
-- incompatibility, file uploads from these clients can result in errors
-- even when the file is otherwise successfully uploaded.
--
-- Set the value to @ENABLE_NO_OP@ to have the Transfer Family server
-- ignore the @SETSTAT@ command, and upload files without needing to make
-- any changes to your SFTP client. While the @SetStatOption@
-- @ENABLE_NO_OP@ setting ignores the error, it does generate a log entry
-- in Amazon CloudWatch Logs, so you can determine when the client is
-- making a @SETSTAT@ call.
--
-- If you want to preserve the original timestamp for your file, and modify
-- other file attributes using @SETSTAT@, you can use Amazon EFS as backend
-- storage with Transfer Family.
protocolDetails_setStatOption :: Lens.Lens' ProtocolDetails (Prelude.Maybe SetStatOption)
protocolDetails_setStatOption = Lens.lens (\ProtocolDetails' {setStatOption} -> setStatOption) (\s@ProtocolDetails' {} a -> s {setStatOption = a} :: ProtocolDetails)

-- | A property used with Transfer Family servers that use the FTPS protocol.
-- TLS Session Resumption provides a mechanism to resume or share a
-- negotiated secret key between the control and data connection for an
-- FTPS session. @TlsSessionResumptionMode@ determines whether or not the
-- server resumes recent, negotiated sessions through a unique session ID.
-- This property is available during @CreateServer@ and @UpdateServer@
-- calls. If a @TlsSessionResumptionMode@ value is not specified during
-- @CreateServer@, it is set to @ENFORCED@ by default.
--
-- -   @DISABLED@: the server does not process TLS session resumption
--     client requests and creates a new TLS session for each request.
--
-- -   @ENABLED@: the server processes and accepts clients that are
--     performing TLS session resumption. The server doesn\'t reject client
--     data connections that do not perform the TLS session resumption
--     client processing.
--
-- -   @ENFORCED@: the server processes and accepts clients that are
--     performing TLS session resumption. The server rejects client data
--     connections that do not perform the TLS session resumption client
--     processing. Before you set the value to @ENFORCED@, test your
--     clients.
--
--     Not all FTPS clients perform TLS session resumption. So, if you
--     choose to enforce TLS session resumption, you prevent any
--     connections from FTPS clients that don\'t perform the protocol
--     negotiation. To determine whether or not you can use the @ENFORCED@
--     value, you need to test your clients.
protocolDetails_tlsSessionResumptionMode :: Lens.Lens' ProtocolDetails (Prelude.Maybe TlsSessionResumptionMode)
protocolDetails_tlsSessionResumptionMode = Lens.lens (\ProtocolDetails' {tlsSessionResumptionMode} -> tlsSessionResumptionMode) (\s@ProtocolDetails' {} a -> s {tlsSessionResumptionMode = a} :: ProtocolDetails)

instance Data.FromJSON ProtocolDetails where
  parseJSON =
    Data.withObject
      "ProtocolDetails"
      ( \x ->
          ProtocolDetails'
            Prelude.<$> (x Data..:? "As2Transports")
            Prelude.<*> (x Data..:? "PassiveIp")
            Prelude.<*> (x Data..:? "SetStatOption")
            Prelude.<*> (x Data..:? "TlsSessionResumptionMode")
      )

instance Prelude.Hashable ProtocolDetails where
  hashWithSalt _salt ProtocolDetails' {..} =
    _salt
      `Prelude.hashWithSalt` as2Transports
      `Prelude.hashWithSalt` passiveIp
      `Prelude.hashWithSalt` setStatOption
      `Prelude.hashWithSalt` tlsSessionResumptionMode

instance Prelude.NFData ProtocolDetails where
  rnf ProtocolDetails' {..} =
    Prelude.rnf as2Transports
      `Prelude.seq` Prelude.rnf passiveIp
      `Prelude.seq` Prelude.rnf setStatOption
      `Prelude.seq` Prelude.rnf tlsSessionResumptionMode

instance Data.ToJSON ProtocolDetails where
  toJSON ProtocolDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("As2Transports" Data..=) Prelude.<$> as2Transports,
            ("PassiveIp" Data..=) Prelude.<$> passiveIp,
            ("SetStatOption" Data..=) Prelude.<$> setStatOption,
            ("TlsSessionResumptionMode" Data..=)
              Prelude.<$> tlsSessionResumptionMode
          ]
      )
