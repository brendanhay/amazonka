{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the filter specified by the filter name.
module Network.AWS.GuardDuty.UpdateFilter
  ( -- * Creating a request
    UpdateFilter (..),
    mkUpdateFilter,

    -- ** Request lenses
    ufFindingCriteria,
    ufAction,
    ufDescription,
    ufRank,
    ufDetectorId,
    ufFilterName,

    -- * Destructuring the response
    UpdateFilterResponse (..),
    mkUpdateFilterResponse,

    -- ** Response lenses
    ufrsResponseStatus,
    ufrsName,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFilter' smart constructor.
data UpdateFilter = UpdateFilter'
  { findingCriteria ::
      Lude.Maybe FindingCriteria,
    action :: Lude.Maybe FilterAction,
    description :: Lude.Maybe Lude.Text,
    rank :: Lude.Maybe Lude.Natural,
    detectorId :: Lude.Text,
    filterName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFilter' with the minimum fields required to make a request.
--
-- * 'action' - Specifies the action that is to be applied to the findings that match the filter.
-- * 'description' - The description of the filter.
-- * 'detectorId' - The unique ID of the detector that specifies the GuardDuty service where you want to update a filter.
-- * 'filterName' - The name of the filter.
-- * 'findingCriteria' - Represents the criteria to be used in the filter for querying findings.
-- * 'rank' - Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
mkUpdateFilter ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'filterName'
  Lude.Text ->
  UpdateFilter
mkUpdateFilter pDetectorId_ pFilterName_ =
  UpdateFilter'
    { findingCriteria = Lude.Nothing,
      action = Lude.Nothing,
      description = Lude.Nothing,
      rank = Lude.Nothing,
      detectorId = pDetectorId_,
      filterName = pFilterName_
    }

-- | Represents the criteria to be used in the filter for querying findings.
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFindingCriteria :: Lens.Lens' UpdateFilter (Lude.Maybe FindingCriteria)
ufFindingCriteria = Lens.lens (findingCriteria :: UpdateFilter -> Lude.Maybe FindingCriteria) (\s a -> s {findingCriteria = a} :: UpdateFilter)
{-# DEPRECATED ufFindingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead." #-}

-- | Specifies the action that is to be applied to the findings that match the filter.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAction :: Lens.Lens' UpdateFilter (Lude.Maybe FilterAction)
ufAction = Lens.lens (action :: UpdateFilter -> Lude.Maybe FilterAction) (\s a -> s {action = a} :: UpdateFilter)
{-# DEPRECATED ufAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The description of the filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFilter (Lude.Maybe Lude.Text)
ufDescription = Lens.lens (description :: UpdateFilter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFilter)
{-# DEPRECATED ufDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the position of the filter in the list of current filters. Also specifies the order in which this filter is applied to the findings.
--
-- /Note:/ Consider using 'rank' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufRank :: Lens.Lens' UpdateFilter (Lude.Maybe Lude.Natural)
ufRank = Lens.lens (rank :: UpdateFilter -> Lude.Maybe Lude.Natural) (\s a -> s {rank = a} :: UpdateFilter)
{-# DEPRECATED ufRank "Use generic-lens or generic-optics with 'rank' instead." #-}

-- | The unique ID of the detector that specifies the GuardDuty service where you want to update a filter.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDetectorId :: Lens.Lens' UpdateFilter Lude.Text
ufDetectorId = Lens.lens (detectorId :: UpdateFilter -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateFilter)
{-# DEPRECATED ufDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufFilterName :: Lens.Lens' UpdateFilter Lude.Text
ufFilterName = Lens.lens (filterName :: UpdateFilter -> Lude.Text) (\s a -> s {filterName = a} :: UpdateFilter)
{-# DEPRECATED ufFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Lude.AWSRequest UpdateFilter where
  type Rs UpdateFilter = UpdateFilterResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateFilterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "name")
      )

instance Lude.ToHeaders UpdateFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFilter where
  toJSON UpdateFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("findingCriteria" Lude..=) Lude.<$> findingCriteria,
            ("action" Lude..=) Lude.<$> action,
            ("description" Lude..=) Lude.<$> description,
            ("rank" Lude..=) Lude.<$> rank
          ]
      )

instance Lude.ToPath UpdateFilter where
  toPath UpdateFilter' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/filter/",
        Lude.toBS filterName
      ]

instance Lude.ToQuery UpdateFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFilterResponse' smart constructor.
data UpdateFilterResponse = UpdateFilterResponse'
  { responseStatus ::
      Lude.Int,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFilterResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the filter.
-- * 'responseStatus' - The response status code.
mkUpdateFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'name'
  Lude.Text ->
  UpdateFilterResponse
mkUpdateFilterResponse pResponseStatus_ pName_ =
  UpdateFilterResponse'
    { responseStatus = pResponseStatus_,
      name = pName_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsResponseStatus :: Lens.Lens' UpdateFilterResponse Lude.Int
ufrsResponseStatus = Lens.lens (responseStatus :: UpdateFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFilterResponse)
{-# DEPRECATED ufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsName :: Lens.Lens' UpdateFilterResponse Lude.Text
ufrsName = Lens.lens (name :: UpdateFilterResponse -> Lude.Text) (\s a -> s {name = a} :: UpdateFilterResponse)
{-# DEPRECATED ufrsName "Use generic-lens or generic-optics with 'name' instead." #-}
