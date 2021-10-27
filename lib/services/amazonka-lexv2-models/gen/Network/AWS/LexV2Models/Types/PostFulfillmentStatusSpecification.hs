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
-- Module      : Network.AWS.LexV2Models.Types.PostFulfillmentStatusSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.PostFulfillmentStatusSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.ResponseSpecification
import qualified Network.AWS.Prelude as Prelude

-- | Provides a setting that determines whether the post-fulfillment response
-- is sent to the user. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/streaming-progress.html#progress-complete>
--
-- /See:/ 'newPostFulfillmentStatusSpecification' smart constructor.
data PostFulfillmentStatusSpecification = PostFulfillmentStatusSpecification'
  { successResponse :: Prelude.Maybe ResponseSpecification,
    timeoutResponse :: Prelude.Maybe ResponseSpecification,
    failureResponse :: Prelude.Maybe ResponseSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostFulfillmentStatusSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successResponse', 'postFulfillmentStatusSpecification_successResponse' - Undocumented member.
--
-- 'timeoutResponse', 'postFulfillmentStatusSpecification_timeoutResponse' - Undocumented member.
--
-- 'failureResponse', 'postFulfillmentStatusSpecification_failureResponse' - Undocumented member.
newPostFulfillmentStatusSpecification ::
  PostFulfillmentStatusSpecification
newPostFulfillmentStatusSpecification =
  PostFulfillmentStatusSpecification'
    { successResponse =
        Prelude.Nothing,
      timeoutResponse = Prelude.Nothing,
      failureResponse = Prelude.Nothing
    }

-- | Undocumented member.
postFulfillmentStatusSpecification_successResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_successResponse = Lens.lens (\PostFulfillmentStatusSpecification' {successResponse} -> successResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {successResponse = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_timeoutResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_timeoutResponse = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutResponse} -> timeoutResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutResponse = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_failureResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_failureResponse = Lens.lens (\PostFulfillmentStatusSpecification' {failureResponse} -> failureResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureResponse = a} :: PostFulfillmentStatusSpecification)

instance
  Core.FromJSON
    PostFulfillmentStatusSpecification
  where
  parseJSON =
    Core.withObject
      "PostFulfillmentStatusSpecification"
      ( \x ->
          PostFulfillmentStatusSpecification'
            Prelude.<$> (x Core..:? "successResponse")
            Prelude.<*> (x Core..:? "timeoutResponse")
            Prelude.<*> (x Core..:? "failureResponse")
      )

instance
  Prelude.Hashable
    PostFulfillmentStatusSpecification

instance
  Prelude.NFData
    PostFulfillmentStatusSpecification

instance
  Core.ToJSON
    PostFulfillmentStatusSpecification
  where
  toJSON PostFulfillmentStatusSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("successResponse" Core..=)
              Prelude.<$> successResponse,
            ("timeoutResponse" Core..=)
              Prelude.<$> timeoutResponse,
            ("failureResponse" Core..=)
              Prelude.<$> failureResponse
          ]
      )
