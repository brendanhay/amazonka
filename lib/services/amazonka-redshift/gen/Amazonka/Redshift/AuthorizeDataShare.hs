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
-- Module      : Amazonka.Redshift.AuthorizeDataShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a data producer account, authorizes the sharing of a datashare with
-- one or more consumer accounts or managing entities. To authorize a
-- datashare for a data consumer, the producer account must have the
-- correct access permissions.
module Amazonka.Redshift.AuthorizeDataShare
  ( -- * Creating a Request
    AuthorizeDataShare (..),
    newAuthorizeDataShare,

    -- * Request Lenses
    authorizeDataShare_dataShareArn,
    authorizeDataShare_consumerIdentifier,

    -- * Destructuring the Response
    DataShare (..),
    newDataShare,

    -- * Response Lenses
    dataShare_dataShareAssociations,
    dataShare_producerArn,
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_managedBy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAuthorizeDataShare' smart constructor.
data AuthorizeDataShare = AuthorizeDataShare'
  { -- | The Amazon Resource Name (ARN) of the datashare that producers are to
    -- authorize sharing for.
    dataShareArn :: Prelude.Text,
    -- | The identifier of the data consumer that is authorized to access the
    -- datashare. This identifier is an Amazon Web Services account ID or a
    -- keyword, such as ADX.
    consumerIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeDataShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShareArn', 'authorizeDataShare_dataShareArn' - The Amazon Resource Name (ARN) of the datashare that producers are to
-- authorize sharing for.
--
-- 'consumerIdentifier', 'authorizeDataShare_consumerIdentifier' - The identifier of the data consumer that is authorized to access the
-- datashare. This identifier is an Amazon Web Services account ID or a
-- keyword, such as ADX.
newAuthorizeDataShare ::
  -- | 'dataShareArn'
  Prelude.Text ->
  -- | 'consumerIdentifier'
  Prelude.Text ->
  AuthorizeDataShare
newAuthorizeDataShare
  pDataShareArn_
  pConsumerIdentifier_ =
    AuthorizeDataShare'
      { dataShareArn = pDataShareArn_,
        consumerIdentifier = pConsumerIdentifier_
      }

-- | The Amazon Resource Name (ARN) of the datashare that producers are to
-- authorize sharing for.
authorizeDataShare_dataShareArn :: Lens.Lens' AuthorizeDataShare Prelude.Text
authorizeDataShare_dataShareArn = Lens.lens (\AuthorizeDataShare' {dataShareArn} -> dataShareArn) (\s@AuthorizeDataShare' {} a -> s {dataShareArn = a} :: AuthorizeDataShare)

-- | The identifier of the data consumer that is authorized to access the
-- datashare. This identifier is an Amazon Web Services account ID or a
-- keyword, such as ADX.
authorizeDataShare_consumerIdentifier :: Lens.Lens' AuthorizeDataShare Prelude.Text
authorizeDataShare_consumerIdentifier = Lens.lens (\AuthorizeDataShare' {consumerIdentifier} -> consumerIdentifier) (\s@AuthorizeDataShare' {} a -> s {consumerIdentifier = a} :: AuthorizeDataShare)

instance Core.AWSRequest AuthorizeDataShare where
  type AWSResponse AuthorizeDataShare = DataShare
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AuthorizeDataShareResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable AuthorizeDataShare where
  hashWithSalt _salt AuthorizeDataShare' {..} =
    _salt `Prelude.hashWithSalt` dataShareArn
      `Prelude.hashWithSalt` consumerIdentifier

instance Prelude.NFData AuthorizeDataShare where
  rnf AuthorizeDataShare' {..} =
    Prelude.rnf dataShareArn
      `Prelude.seq` Prelude.rnf consumerIdentifier

instance Core.ToHeaders AuthorizeDataShare where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AuthorizeDataShare where
  toPath = Prelude.const "/"

instance Core.ToQuery AuthorizeDataShare where
  toQuery AuthorizeDataShare' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AuthorizeDataShare" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "DataShareArn" Core.=: dataShareArn,
        "ConsumerIdentifier" Core.=: consumerIdentifier
      ]
