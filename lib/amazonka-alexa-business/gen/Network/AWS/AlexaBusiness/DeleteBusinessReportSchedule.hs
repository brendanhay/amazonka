{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the recurring report delivery schedule with the specified schedule ARN.
module Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
  ( -- * Creating a request
    DeleteBusinessReportSchedule (..),
    mkDeleteBusinessReportSchedule,

    -- ** Request lenses
    dbrsScheduleARN,

    -- * Destructuring the response
    DeleteBusinessReportScheduleResponse (..),
    mkDeleteBusinessReportScheduleResponse,

    -- ** Response lenses
    dbrsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBusinessReportSchedule' smart constructor.
newtype DeleteBusinessReportSchedule = DeleteBusinessReportSchedule'
  { -- | The ARN of the business report schedule.
    scheduleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBusinessReportSchedule' with the minimum fields required to make a request.
--
-- * 'scheduleARN' - The ARN of the business report schedule.
mkDeleteBusinessReportSchedule ::
  -- | 'scheduleARN'
  Lude.Text ->
  DeleteBusinessReportSchedule
mkDeleteBusinessReportSchedule pScheduleARN_ =
  DeleteBusinessReportSchedule' {scheduleARN = pScheduleARN_}

-- | The ARN of the business report schedule.
--
-- /Note:/ Consider using 'scheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsScheduleARN :: Lens.Lens' DeleteBusinessReportSchedule Lude.Text
dbrsScheduleARN = Lens.lens (scheduleARN :: DeleteBusinessReportSchedule -> Lude.Text) (\s a -> s {scheduleARN = a} :: DeleteBusinessReportSchedule)
{-# DEPRECATED dbrsScheduleARN "Use generic-lens or generic-optics with 'scheduleARN' instead." #-}

instance Lude.AWSRequest DeleteBusinessReportSchedule where
  type
    Rs DeleteBusinessReportSchedule =
      DeleteBusinessReportScheduleResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteBusinessReportScheduleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBusinessReportSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.DeleteBusinessReportSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBusinessReportSchedule where
  toJSON DeleteBusinessReportSchedule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ScheduleArn" Lude..= scheduleARN)])

instance Lude.ToPath DeleteBusinessReportSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBusinessReportSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBusinessReportScheduleResponse' smart constructor.
newtype DeleteBusinessReportScheduleResponse = DeleteBusinessReportScheduleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBusinessReportScheduleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteBusinessReportScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBusinessReportScheduleResponse
mkDeleteBusinessReportScheduleResponse pResponseStatus_ =
  DeleteBusinessReportScheduleResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsrsResponseStatus :: Lens.Lens' DeleteBusinessReportScheduleResponse Lude.Int
dbrsrsResponseStatus = Lens.lens (responseStatus :: DeleteBusinessReportScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBusinessReportScheduleResponse)
{-# DEPRECATED dbrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
