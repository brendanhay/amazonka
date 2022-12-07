{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElastiCache.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a cache security group. Use this operation to
-- disallow access from an Amazon EC2 security group that had been
-- previously authorized.
module Amazonka.ElastiCache.RevokeCacheSecurityGroupIngress
  ( -- * Creating a Request
    RevokeCacheSecurityGroupIngress (..),
    newRevokeCacheSecurityGroupIngress,

    -- * Request Lenses
    revokeCacheSecurityGroupIngress_cacheSecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupName,
    revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId,

    -- * Destructuring the Response
    RevokeCacheSecurityGroupIngressResponse (..),
    newRevokeCacheSecurityGroupIngressResponse,

    -- * Response Lenses
    revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup,
    revokeCacheSecurityGroupIngressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @RevokeCacheSecurityGroupIngress@ operation.
--
-- /See:/ 'newRevokeCacheSecurityGroupIngress' smart constructor.
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'
  { -- | The name of the cache security group to revoke ingress from.
    cacheSecurityGroupName :: Prelude.Text,
    -- | The name of the Amazon EC2 security group to revoke access from.
    eC2SecurityGroupName :: Prelude.Text,
    -- | The Amazon account number of the Amazon EC2 security group owner. Note
    -- that this is not the same thing as an Amazon access key ID - you must
    -- provide a valid Amazon account number for this parameter.
    eC2SecurityGroupOwnerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeCacheSecurityGroupIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroupName', 'revokeCacheSecurityGroupIngress_cacheSecurityGroupName' - The name of the cache security group to revoke ingress from.
--
-- 'eC2SecurityGroupName', 'revokeCacheSecurityGroupIngress_eC2SecurityGroupName' - The name of the Amazon EC2 security group to revoke access from.
--
-- 'eC2SecurityGroupOwnerId', 'revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId' - The Amazon account number of the Amazon EC2 security group owner. Note
-- that this is not the same thing as an Amazon access key ID - you must
-- provide a valid Amazon account number for this parameter.
newRevokeCacheSecurityGroupIngress ::
  -- | 'cacheSecurityGroupName'
  Prelude.Text ->
  -- | 'eC2SecurityGroupName'
  Prelude.Text ->
  -- | 'eC2SecurityGroupOwnerId'
  Prelude.Text ->
  RevokeCacheSecurityGroupIngress
newRevokeCacheSecurityGroupIngress
  pCacheSecurityGroupName_
  pEC2SecurityGroupName_
  pEC2SecurityGroupOwnerId_ =
    RevokeCacheSecurityGroupIngress'
      { cacheSecurityGroupName =
          pCacheSecurityGroupName_,
        eC2SecurityGroupName =
          pEC2SecurityGroupName_,
        eC2SecurityGroupOwnerId =
          pEC2SecurityGroupOwnerId_
      }

-- | The name of the cache security group to revoke ingress from.
revokeCacheSecurityGroupIngress_cacheSecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Prelude.Text
revokeCacheSecurityGroupIngress_cacheSecurityGroupName = Lens.lens (\RevokeCacheSecurityGroupIngress' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@RevokeCacheSecurityGroupIngress' {} a -> s {cacheSecurityGroupName = a} :: RevokeCacheSecurityGroupIngress)

-- | The name of the Amazon EC2 security group to revoke access from.
revokeCacheSecurityGroupIngress_eC2SecurityGroupName :: Lens.Lens' RevokeCacheSecurityGroupIngress Prelude.Text
revokeCacheSecurityGroupIngress_eC2SecurityGroupName = Lens.lens (\RevokeCacheSecurityGroupIngress' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@RevokeCacheSecurityGroupIngress' {} a -> s {eC2SecurityGroupName = a} :: RevokeCacheSecurityGroupIngress)

-- | The Amazon account number of the Amazon EC2 security group owner. Note
-- that this is not the same thing as an Amazon access key ID - you must
-- provide a valid Amazon account number for this parameter.
revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId :: Lens.Lens' RevokeCacheSecurityGroupIngress Prelude.Text
revokeCacheSecurityGroupIngress_eC2SecurityGroupOwnerId = Lens.lens (\RevokeCacheSecurityGroupIngress' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@RevokeCacheSecurityGroupIngress' {} a -> s {eC2SecurityGroupOwnerId = a} :: RevokeCacheSecurityGroupIngress)

instance
  Core.AWSRequest
    RevokeCacheSecurityGroupIngress
  where
  type
    AWSResponse RevokeCacheSecurityGroupIngress =
      RevokeCacheSecurityGroupIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RevokeCacheSecurityGroupIngressResult"
      ( \s h x ->
          RevokeCacheSecurityGroupIngressResponse'
            Prelude.<$> (x Data..@? "CacheSecurityGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RevokeCacheSecurityGroupIngress
  where
  hashWithSalt
    _salt
    RevokeCacheSecurityGroupIngress' {..} =
      _salt `Prelude.hashWithSalt` cacheSecurityGroupName
        `Prelude.hashWithSalt` eC2SecurityGroupName
        `Prelude.hashWithSalt` eC2SecurityGroupOwnerId

instance
  Prelude.NFData
    RevokeCacheSecurityGroupIngress
  where
  rnf RevokeCacheSecurityGroupIngress' {..} =
    Prelude.rnf cacheSecurityGroupName
      `Prelude.seq` Prelude.rnf eC2SecurityGroupName
      `Prelude.seq` Prelude.rnf eC2SecurityGroupOwnerId

instance
  Data.ToHeaders
    RevokeCacheSecurityGroupIngress
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RevokeCacheSecurityGroupIngress where
  toPath = Prelude.const "/"

instance Data.ToQuery RevokeCacheSecurityGroupIngress where
  toQuery RevokeCacheSecurityGroupIngress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RevokeCacheSecurityGroupIngress" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheSecurityGroupName"
          Data.=: cacheSecurityGroupName,
        "EC2SecurityGroupName" Data.=: eC2SecurityGroupName,
        "EC2SecurityGroupOwnerId"
          Data.=: eC2SecurityGroupOwnerId
      ]

-- | /See:/ 'newRevokeCacheSecurityGroupIngressResponse' smart constructor.
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'
  { cacheSecurityGroup :: Prelude.Maybe CacheSecurityGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeCacheSecurityGroupIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroup', 'revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup' - Undocumented member.
--
-- 'httpStatus', 'revokeCacheSecurityGroupIngressResponse_httpStatus' - The response's http status code.
newRevokeCacheSecurityGroupIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeCacheSecurityGroupIngressResponse
newRevokeCacheSecurityGroupIngressResponse
  pHttpStatus_ =
    RevokeCacheSecurityGroupIngressResponse'
      { cacheSecurityGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse (Prelude.Maybe CacheSecurityGroup)
revokeCacheSecurityGroupIngressResponse_cacheSecurityGroup = Lens.lens (\RevokeCacheSecurityGroupIngressResponse' {cacheSecurityGroup} -> cacheSecurityGroup) (\s@RevokeCacheSecurityGroupIngressResponse' {} a -> s {cacheSecurityGroup = a} :: RevokeCacheSecurityGroupIngressResponse)

-- | The response's http status code.
revokeCacheSecurityGroupIngressResponse_httpStatus :: Lens.Lens' RevokeCacheSecurityGroupIngressResponse Prelude.Int
revokeCacheSecurityGroupIngressResponse_httpStatus = Lens.lens (\RevokeCacheSecurityGroupIngressResponse' {httpStatus} -> httpStatus) (\s@RevokeCacheSecurityGroupIngressResponse' {} a -> s {httpStatus = a} :: RevokeCacheSecurityGroupIngressResponse)

instance
  Prelude.NFData
    RevokeCacheSecurityGroupIngressResponse
  where
  rnf RevokeCacheSecurityGroupIngressResponse' {..} =
    Prelude.rnf cacheSecurityGroup
      `Prelude.seq` Prelude.rnf httpStatus
