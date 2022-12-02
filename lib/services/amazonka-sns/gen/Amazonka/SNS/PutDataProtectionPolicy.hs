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
-- Module      : Amazonka.SNS.PutDataProtectionPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is stored in the
-- specified Amazon SNS topic.
module Amazonka.SNS.PutDataProtectionPolicy
  ( -- * Creating a Request
    PutDataProtectionPolicy (..),
    newPutDataProtectionPolicy,

    -- * Request Lenses
    putDataProtectionPolicy_resourceArn,
    putDataProtectionPolicy_dataProtectionPolicy,

    -- * Destructuring the Response
    PutDataProtectionPolicyResponse (..),
    newPutDataProtectionPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newPutDataProtectionPolicy' smart constructor.
data PutDataProtectionPolicy = PutDataProtectionPolicy'
  { -- | The ARN of the topic whose @DataProtectionPolicy@ you want to add or
    -- update.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the Amazon Web Services General Reference.
    resourceArn :: Prelude.Text,
    -- | The JSON serialization of the topic\'s @DataProtectionPolicy@.
    --
    -- The @DataProtectionPolicy@ must be in JSON string format.
    --
    -- Length Constraints: Maximum length of 30,720.
    dataProtectionPolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataProtectionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putDataProtectionPolicy_resourceArn' - The ARN of the topic whose @DataProtectionPolicy@ you want to add or
-- update.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the Amazon Web Services General Reference.
--
-- 'dataProtectionPolicy', 'putDataProtectionPolicy_dataProtectionPolicy' - The JSON serialization of the topic\'s @DataProtectionPolicy@.
--
-- The @DataProtectionPolicy@ must be in JSON string format.
--
-- Length Constraints: Maximum length of 30,720.
newPutDataProtectionPolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'dataProtectionPolicy'
  Prelude.Text ->
  PutDataProtectionPolicy
newPutDataProtectionPolicy
  pResourceArn_
  pDataProtectionPolicy_ =
    PutDataProtectionPolicy'
      { resourceArn =
          pResourceArn_,
        dataProtectionPolicy = pDataProtectionPolicy_
      }

-- | The ARN of the topic whose @DataProtectionPolicy@ you want to add or
-- update.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the Amazon Web Services General Reference.
putDataProtectionPolicy_resourceArn :: Lens.Lens' PutDataProtectionPolicy Prelude.Text
putDataProtectionPolicy_resourceArn = Lens.lens (\PutDataProtectionPolicy' {resourceArn} -> resourceArn) (\s@PutDataProtectionPolicy' {} a -> s {resourceArn = a} :: PutDataProtectionPolicy)

-- | The JSON serialization of the topic\'s @DataProtectionPolicy@.
--
-- The @DataProtectionPolicy@ must be in JSON string format.
--
-- Length Constraints: Maximum length of 30,720.
putDataProtectionPolicy_dataProtectionPolicy :: Lens.Lens' PutDataProtectionPolicy Prelude.Text
putDataProtectionPolicy_dataProtectionPolicy = Lens.lens (\PutDataProtectionPolicy' {dataProtectionPolicy} -> dataProtectionPolicy) (\s@PutDataProtectionPolicy' {} a -> s {dataProtectionPolicy = a} :: PutDataProtectionPolicy)

instance Core.AWSRequest PutDataProtectionPolicy where
  type
    AWSResponse PutDataProtectionPolicy =
      PutDataProtectionPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      PutDataProtectionPolicyResponse'

instance Prelude.Hashable PutDataProtectionPolicy where
  hashWithSalt _salt PutDataProtectionPolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` dataProtectionPolicy

instance Prelude.NFData PutDataProtectionPolicy where
  rnf PutDataProtectionPolicy' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf dataProtectionPolicy

instance Data.ToHeaders PutDataProtectionPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutDataProtectionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutDataProtectionPolicy where
  toQuery PutDataProtectionPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutDataProtectionPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "ResourceArn" Data.=: resourceArn,
        "DataProtectionPolicy" Data.=: dataProtectionPolicy
      ]

-- | /See:/ 'newPutDataProtectionPolicyResponse' smart constructor.
data PutDataProtectionPolicyResponse = PutDataProtectionPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataProtectionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutDataProtectionPolicyResponse ::
  PutDataProtectionPolicyResponse
newPutDataProtectionPolicyResponse =
  PutDataProtectionPolicyResponse'

instance
  Prelude.NFData
    PutDataProtectionPolicyResponse
  where
  rnf _ = ()
