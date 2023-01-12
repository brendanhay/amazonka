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
-- Module      : Amazonka.WAF.CreateIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Creates an IPSet, which you use to specify which web requests that you
-- want to allow or block based on the IP addresses that the requests
-- originate from. For example, if you\'re receiving a lot of requests from
-- one or more individual IP addresses or one or more ranges of IP
-- addresses and you want to block the requests, you can create an @IPSet@
-- that contains those IP addresses and then configure AWS WAF to block the
-- requests.
--
-- To create and configure an @IPSet@, perform the following steps:
--
-- 1.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateIPSet@ request.
--
-- 2.  Submit a @CreateIPSet@ request.
--
-- 3.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateIPSet request.
--
-- 4.  Submit an @UpdateIPSet@ request to specify the IP addresses that you
--     want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAF.CreateIPSet
  ( -- * Creating a Request
    CreateIPSet (..),
    newCreateIPSet,

    -- * Request Lenses
    createIPSet_name,
    createIPSet_changeToken,

    -- * Destructuring the Response
    CreateIPSetResponse (..),
    newCreateIPSetResponse,

    -- * Response Lenses
    createIPSetResponse_changeToken,
    createIPSetResponse_iPSet,
    createIPSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newCreateIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { -- | A friendly name or description of the IPSet. You can\'t change @Name@
    -- after you create the @IPSet@.
    name :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createIPSet_name' - A friendly name or description of the IPSet. You can\'t change @Name@
-- after you create the @IPSet@.
--
-- 'changeToken', 'createIPSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateIPSet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateIPSet
newCreateIPSet pName_ pChangeToken_ =
  CreateIPSet'
    { name = pName_,
      changeToken = pChangeToken_
    }

-- | A friendly name or description of the IPSet. You can\'t change @Name@
-- after you create the @IPSet@.
createIPSet_name :: Lens.Lens' CreateIPSet Prelude.Text
createIPSet_name = Lens.lens (\CreateIPSet' {name} -> name) (\s@CreateIPSet' {} a -> s {name = a} :: CreateIPSet)

-- | The value returned by the most recent call to GetChangeToken.
createIPSet_changeToken :: Lens.Lens' CreateIPSet Prelude.Text
createIPSet_changeToken = Lens.lens (\CreateIPSet' {changeToken} -> changeToken) (\s@CreateIPSet' {} a -> s {changeToken = a} :: CreateIPSet)

instance Core.AWSRequest CreateIPSet where
  type AWSResponse CreateIPSet = CreateIPSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIPSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (x Data..?> "IPSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateIPSet where
  hashWithSalt _salt CreateIPSet' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateIPSet where
  rnf CreateIPSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders CreateIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.CreateIPSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateIPSet where
  toJSON CreateIPSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath CreateIPSet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The IPSet returned in the @CreateIPSet@ response.
    iPSet :: Prelude.Maybe IPSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'createIPSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'iPSet', 'createIPSetResponse_iPSet' - The IPSet returned in the @CreateIPSet@ response.
--
-- 'httpStatus', 'createIPSetResponse_httpStatus' - The response's http status code.
newCreateIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateIPSetResponse
newCreateIPSetResponse pHttpStatus_ =
  CreateIPSetResponse'
    { changeToken = Prelude.Nothing,
      iPSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @CreateIPSet@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
createIPSetResponse_changeToken :: Lens.Lens' CreateIPSetResponse (Prelude.Maybe Prelude.Text)
createIPSetResponse_changeToken = Lens.lens (\CreateIPSetResponse' {changeToken} -> changeToken) (\s@CreateIPSetResponse' {} a -> s {changeToken = a} :: CreateIPSetResponse)

-- | The IPSet returned in the @CreateIPSet@ response.
createIPSetResponse_iPSet :: Lens.Lens' CreateIPSetResponse (Prelude.Maybe IPSet)
createIPSetResponse_iPSet = Lens.lens (\CreateIPSetResponse' {iPSet} -> iPSet) (\s@CreateIPSetResponse' {} a -> s {iPSet = a} :: CreateIPSetResponse)

-- | The response's http status code.
createIPSetResponse_httpStatus :: Lens.Lens' CreateIPSetResponse Prelude.Int
createIPSetResponse_httpStatus = Lens.lens (\CreateIPSetResponse' {httpStatus} -> httpStatus) (\s@CreateIPSetResponse' {} a -> s {httpStatus = a} :: CreateIPSetResponse)

instance Prelude.NFData CreateIPSetResponse where
  rnf CreateIPSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf iPSet
      `Prelude.seq` Prelude.rnf httpStatus
