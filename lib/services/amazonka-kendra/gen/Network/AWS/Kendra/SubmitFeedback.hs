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
-- Module      : Network.AWS.Kendra.SubmitFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables you to provide feedback to Amazon Kendra to improve the
-- performance of your index.
module Network.AWS.Kendra.SubmitFeedback
  ( -- * Creating a Request
    SubmitFeedback (..),
    newSubmitFeedback,

    -- * Request Lenses
    submitFeedback_relevanceFeedbackItems,
    submitFeedback_clickFeedbackItems,
    submitFeedback_indexId,
    submitFeedback_queryId,

    -- * Destructuring the Response
    SubmitFeedbackResponse (..),
    newSubmitFeedbackResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSubmitFeedback' smart constructor.
data SubmitFeedback = SubmitFeedback'
  { -- | Provides Amazon Kendra with relevant or not relevant feedback for
    -- whether a particular item was relevant to the search.
    relevanceFeedbackItems :: Prelude.Maybe [RelevanceFeedback],
    -- | Tells Amazon Kendra that a particular search result link was chosen by
    -- the user.
    clickFeedbackItems :: Prelude.Maybe [ClickFeedback],
    -- | The identifier of the index that was queried.
    indexId :: Prelude.Text,
    -- | The identifier of the specific query for which you are submitting
    -- feedback. The query ID is returned in the response to the @Query@
    -- operation.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relevanceFeedbackItems', 'submitFeedback_relevanceFeedbackItems' - Provides Amazon Kendra with relevant or not relevant feedback for
-- whether a particular item was relevant to the search.
--
-- 'clickFeedbackItems', 'submitFeedback_clickFeedbackItems' - Tells Amazon Kendra that a particular search result link was chosen by
-- the user.
--
-- 'indexId', 'submitFeedback_indexId' - The identifier of the index that was queried.
--
-- 'queryId', 'submitFeedback_queryId' - The identifier of the specific query for which you are submitting
-- feedback. The query ID is returned in the response to the @Query@
-- operation.
newSubmitFeedback ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'queryId'
  Prelude.Text ->
  SubmitFeedback
newSubmitFeedback pIndexId_ pQueryId_ =
  SubmitFeedback'
    { relevanceFeedbackItems =
        Prelude.Nothing,
      clickFeedbackItems = Prelude.Nothing,
      indexId = pIndexId_,
      queryId = pQueryId_
    }

-- | Provides Amazon Kendra with relevant or not relevant feedback for
-- whether a particular item was relevant to the search.
submitFeedback_relevanceFeedbackItems :: Lens.Lens' SubmitFeedback (Prelude.Maybe [RelevanceFeedback])
submitFeedback_relevanceFeedbackItems = Lens.lens (\SubmitFeedback' {relevanceFeedbackItems} -> relevanceFeedbackItems) (\s@SubmitFeedback' {} a -> s {relevanceFeedbackItems = a} :: SubmitFeedback) Prelude.. Lens.mapping Lens.coerced

-- | Tells Amazon Kendra that a particular search result link was chosen by
-- the user.
submitFeedback_clickFeedbackItems :: Lens.Lens' SubmitFeedback (Prelude.Maybe [ClickFeedback])
submitFeedback_clickFeedbackItems = Lens.lens (\SubmitFeedback' {clickFeedbackItems} -> clickFeedbackItems) (\s@SubmitFeedback' {} a -> s {clickFeedbackItems = a} :: SubmitFeedback) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index that was queried.
submitFeedback_indexId :: Lens.Lens' SubmitFeedback Prelude.Text
submitFeedback_indexId = Lens.lens (\SubmitFeedback' {indexId} -> indexId) (\s@SubmitFeedback' {} a -> s {indexId = a} :: SubmitFeedback)

-- | The identifier of the specific query for which you are submitting
-- feedback. The query ID is returned in the response to the @Query@
-- operation.
submitFeedback_queryId :: Lens.Lens' SubmitFeedback Prelude.Text
submitFeedback_queryId = Lens.lens (\SubmitFeedback' {queryId} -> queryId) (\s@SubmitFeedback' {} a -> s {queryId = a} :: SubmitFeedback)

instance Core.AWSRequest SubmitFeedback where
  type
    AWSResponse SubmitFeedback =
      SubmitFeedbackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SubmitFeedbackResponse'

instance Prelude.Hashable SubmitFeedback

instance Prelude.NFData SubmitFeedback

instance Core.ToHeaders SubmitFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.SubmitFeedback" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SubmitFeedback where
  toJSON SubmitFeedback' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RelevanceFeedbackItems" Core..=)
              Prelude.<$> relevanceFeedbackItems,
            ("ClickFeedbackItems" Core..=)
              Prelude.<$> clickFeedbackItems,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("QueryId" Core..= queryId)
          ]
      )

instance Core.ToPath SubmitFeedback where
  toPath = Prelude.const "/"

instance Core.ToQuery SubmitFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitFeedbackResponse' smart constructor.
data SubmitFeedbackResponse = SubmitFeedbackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSubmitFeedbackResponse ::
  SubmitFeedbackResponse
newSubmitFeedbackResponse = SubmitFeedbackResponse'

instance Prelude.NFData SubmitFeedbackResponse
