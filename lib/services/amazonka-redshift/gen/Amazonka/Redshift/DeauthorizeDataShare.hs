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
-- Module      : Amazonka.Redshift.DeauthorizeDataShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a datashare producer account, removes authorization from the
-- specified datashare.
module Amazonka.Redshift.DeauthorizeDataShare
  ( -- * Creating a Request
    DeauthorizeDataShare (..),
    newDeauthorizeDataShare,

    -- * Request Lenses
    deauthorizeDataShare_dataShareArn,
    deauthorizeDataShare_consumerIdentifier,

    -- * Destructuring the Response
    DataShare (..),
    newDataShare,

    -- * Response Lenses
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeauthorizeDataShare' smart constructor.
data DeauthorizeDataShare = DeauthorizeDataShare'
  { -- | The Amazon Resource Name (ARN) of the datashare to remove authorization
    -- from.
    dataShareArn :: Prelude.Text,
    -- | The identifier of the data consumer that is to have authorization
    -- removed from the datashare. This identifier is an Amazon Web Services
    -- account ID or a keyword, such as ADX.
    consumerIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeauthorizeDataShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShareArn', 'deauthorizeDataShare_dataShareArn' - The Amazon Resource Name (ARN) of the datashare to remove authorization
-- from.
--
-- 'consumerIdentifier', 'deauthorizeDataShare_consumerIdentifier' - The identifier of the data consumer that is to have authorization
-- removed from the datashare. This identifier is an Amazon Web Services
-- account ID or a keyword, such as ADX.
newDeauthorizeDataShare ::
  -- | 'dataShareArn'
  Prelude.Text ->
  -- | 'consumerIdentifier'
  Prelude.Text ->
  DeauthorizeDataShare
newDeauthorizeDataShare
  pDataShareArn_
  pConsumerIdentifier_ =
    DeauthorizeDataShare'
      { dataShareArn =
          pDataShareArn_,
        consumerIdentifier = pConsumerIdentifier_
      }

-- | The Amazon Resource Name (ARN) of the datashare to remove authorization
-- from.
deauthorizeDataShare_dataShareArn :: Lens.Lens' DeauthorizeDataShare Prelude.Text
deauthorizeDataShare_dataShareArn = Lens.lens (\DeauthorizeDataShare' {dataShareArn} -> dataShareArn) (\s@DeauthorizeDataShare' {} a -> s {dataShareArn = a} :: DeauthorizeDataShare)

-- | The identifier of the data consumer that is to have authorization
-- removed from the datashare. This identifier is an Amazon Web Services
-- account ID or a keyword, such as ADX.
deauthorizeDataShare_consumerIdentifier :: Lens.Lens' DeauthorizeDataShare Prelude.Text
deauthorizeDataShare_consumerIdentifier = Lens.lens (\DeauthorizeDataShare' {consumerIdentifier} -> consumerIdentifier) (\s@DeauthorizeDataShare' {} a -> s {consumerIdentifier = a} :: DeauthorizeDataShare)

instance Core.AWSRequest DeauthorizeDataShare where
  type AWSResponse DeauthorizeDataShare = DataShare
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeauthorizeDataShareResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable DeauthorizeDataShare where
  hashWithSalt _salt DeauthorizeDataShare' {..} =
    _salt
      `Prelude.hashWithSalt` dataShareArn
      `Prelude.hashWithSalt` consumerIdentifier

instance Prelude.NFData DeauthorizeDataShare where
  rnf DeauthorizeDataShare' {..} =
    Prelude.rnf dataShareArn `Prelude.seq`
      Prelude.rnf consumerIdentifier

instance Data.ToHeaders DeauthorizeDataShare where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeauthorizeDataShare where
  toPath = Prelude.const "/"

instance Data.ToQuery DeauthorizeDataShare where
  toQuery DeauthorizeDataShare' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeauthorizeDataShare" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "DataShareArn" Data.=: dataShareArn,
        "ConsumerIdentifier" Data.=: consumerIdentifier
      ]
