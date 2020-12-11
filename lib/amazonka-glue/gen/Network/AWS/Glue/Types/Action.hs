-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Action
  ( Action (..),

    -- * Smart constructor
    mkAction,

    -- * Lenses
    aNotificationProperty,
    aArguments,
    aJobName,
    aSecurityConfiguration,
    aTimeout,
    aCrawlerName,
  )
where

import Network.AWS.Glue.Types.NotificationProperty
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines an action to be initiated by a trigger.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { notificationProperty ::
      Lude.Maybe NotificationProperty,
    arguments :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    jobName :: Lude.Maybe Lude.Text,
    securityConfiguration :: Lude.Maybe Lude.Text,
    timeout :: Lude.Maybe Lude.Natural,
    crawlerName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- * 'arguments' - The job arguments used when this trigger fires. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
-- * 'crawlerName' - The name of the crawler to be used with this action.
-- * 'jobName' - The name of a job to be executed.
-- * 'notificationProperty' - Specifies configuration properties of a job run notification.
-- * 'securityConfiguration' - The name of the @SecurityConfiguration@ structure to be used with this action.
-- * 'timeout' - The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
mkAction ::
  Action
mkAction =
  Action'
    { notificationProperty = Lude.Nothing,
      arguments = Lude.Nothing,
      jobName = Lude.Nothing,
      securityConfiguration = Lude.Nothing,
      timeout = Lude.Nothing,
      crawlerName = Lude.Nothing
    }

-- | Specifies configuration properties of a job run notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationProperty :: Lens.Lens' Action (Lude.Maybe NotificationProperty)
aNotificationProperty = Lens.lens (notificationProperty :: Action -> Lude.Maybe NotificationProperty) (\s a -> s {notificationProperty = a} :: Action)
{-# DEPRECATED aNotificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead." #-}

-- | The job arguments used when this trigger fires. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArguments :: Lens.Lens' Action (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aArguments = Lens.lens (arguments :: Action -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {arguments = a} :: Action)
{-# DEPRECATED aArguments "Use generic-lens or generic-optics with 'arguments' instead." #-}

-- | The name of a job to be executed.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aJobName :: Lens.Lens' Action (Lude.Maybe Lude.Text)
aJobName = Lens.lens (jobName :: Action -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: Action)
{-# DEPRECATED aJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this action.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSecurityConfiguration :: Lens.Lens' Action (Lude.Maybe Lude.Text)
aSecurityConfiguration = Lens.lens (securityConfiguration :: Action -> Lude.Maybe Lude.Text) (\s a -> s {securityConfiguration = a} :: Action)
{-# DEPRECATED aSecurityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead." #-}

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimeout :: Lens.Lens' Action (Lude.Maybe Lude.Natural)
aTimeout = Lens.lens (timeout :: Action -> Lude.Maybe Lude.Natural) (\s a -> s {timeout = a} :: Action)
{-# DEPRECATED aTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The name of the crawler to be used with this action.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCrawlerName :: Lens.Lens' Action (Lude.Maybe Lude.Text)
aCrawlerName = Lens.lens (crawlerName :: Action -> Lude.Maybe Lude.Text) (\s a -> s {crawlerName = a} :: Action)
{-# DEPRECATED aCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Lude.FromJSON Action where
  parseJSON =
    Lude.withObject
      "Action"
      ( \x ->
          Action'
            Lude.<$> (x Lude..:? "NotificationProperty")
            Lude.<*> (x Lude..:? "Arguments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "SecurityConfiguration")
            Lude.<*> (x Lude..:? "Timeout")
            Lude.<*> (x Lude..:? "CrawlerName")
      )

instance Lude.ToJSON Action where
  toJSON Action' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationProperty" Lude..=) Lude.<$> notificationProperty,
            ("Arguments" Lude..=) Lude.<$> arguments,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("SecurityConfiguration" Lude..=) Lude.<$> securityConfiguration,
            ("Timeout" Lude..=) Lude.<$> timeout,
            ("CrawlerName" Lude..=) Lude.<$> crawlerName
          ]
      )
