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
-- Module      : Amazonka.ResourceGroups.Untag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes tags from a specified resource group.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:Untag@
module Amazonka.ResourceGroups.Untag
  ( -- * Creating a Request
    Untag (..),
    newUntag,

    -- * Request Lenses
    untag_arn,
    untag_keys,

    -- * Destructuring the Response
    UntagResponse (..),
    newUntagResponse,

    -- * Response Lenses
    untagResponse_arn,
    untagResponse_keys,
    untagResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntag' smart constructor.
data Untag = Untag'
  { -- | The ARN of the resource group from which to remove tags. The command
    -- removed both the specified keys and any values associated with those
    -- keys.
    arn :: Prelude.Text,
    -- | The keys of the tags to be removed.
    keys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Untag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'untag_arn' - The ARN of the resource group from which to remove tags. The command
-- removed both the specified keys and any values associated with those
-- keys.
--
-- 'keys', 'untag_keys' - The keys of the tags to be removed.
newUntag ::
  -- | 'arn'
  Prelude.Text ->
  Untag
newUntag pArn_ =
  Untag' {arn = pArn_, keys = Prelude.mempty}

-- | The ARN of the resource group from which to remove tags. The command
-- removed both the specified keys and any values associated with those
-- keys.
untag_arn :: Lens.Lens' Untag Prelude.Text
untag_arn = Lens.lens (\Untag' {arn} -> arn) (\s@Untag' {} a -> s {arn = a} :: Untag)

-- | The keys of the tags to be removed.
untag_keys :: Lens.Lens' Untag [Prelude.Text]
untag_keys = Lens.lens (\Untag' {keys} -> keys) (\s@Untag' {} a -> s {keys = a} :: Untag) Prelude.. Lens.coerced

instance Core.AWSRequest Untag where
  type AWSResponse Untag = UntagResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UntagResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Keys" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable Untag where
  hashWithSalt _salt Untag' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` keys

instance Prelude.NFData Untag where
  rnf Untag' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf keys

instance Core.ToHeaders Untag where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON Untag where
  toJSON Untag' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Keys" Core..= keys)]
      )

instance Core.ToPath Untag where
  toPath Untag' {..} =
    Prelude.mconcat
      ["/resources/", Core.toBS arn, "/tags"]

instance Core.ToQuery Untag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagResponse' smart constructor.
data UntagResponse = UntagResponse'
  { -- | The ARN of the resource group from which tags have been removed.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The keys of the tags that were removed.
    keys :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'untagResponse_arn' - The ARN of the resource group from which tags have been removed.
--
-- 'keys', 'untagResponse_keys' - The keys of the tags that were removed.
--
-- 'httpStatus', 'untagResponse_httpStatus' - The response's http status code.
newUntagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UntagResponse
newUntagResponse pHttpStatus_ =
  UntagResponse'
    { arn = Prelude.Nothing,
      keys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the resource group from which tags have been removed.
untagResponse_arn :: Lens.Lens' UntagResponse (Prelude.Maybe Prelude.Text)
untagResponse_arn = Lens.lens (\UntagResponse' {arn} -> arn) (\s@UntagResponse' {} a -> s {arn = a} :: UntagResponse)

-- | The keys of the tags that were removed.
untagResponse_keys :: Lens.Lens' UntagResponse (Prelude.Maybe [Prelude.Text])
untagResponse_keys = Lens.lens (\UntagResponse' {keys} -> keys) (\s@UntagResponse' {} a -> s {keys = a} :: UntagResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
untagResponse_httpStatus :: Lens.Lens' UntagResponse Prelude.Int
untagResponse_httpStatus = Lens.lens (\UntagResponse' {httpStatus} -> httpStatus) (\s@UntagResponse' {} a -> s {httpStatus = a} :: UntagResponse)

instance Prelude.NFData UntagResponse where
  rnf UntagResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf keys
      `Prelude.seq` Prelude.rnf httpStatus
