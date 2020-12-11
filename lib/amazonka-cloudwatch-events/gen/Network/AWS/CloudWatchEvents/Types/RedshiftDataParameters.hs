-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
  ( RedshiftDataParameters (..),

    -- * Smart constructor
    mkRedshiftDataParameters,

    -- * Lenses
    rdpDBUser,
    rdpSecretManagerARN,
    rdpStatementName,
    rdpWithEvent,
    rdpDatabase,
    rdpSql,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | These are custom parameters to be used when the target is a Redshift cluster to invoke the Redshift Data API ExecuteStatement based on EventBridge events.
--
-- /See:/ 'mkRedshiftDataParameters' smart constructor.
data RedshiftDataParameters = RedshiftDataParameters'
  { dbUser ::
      Lude.Maybe Lude.Text,
    secretManagerARN :: Lude.Maybe Lude.Text,
    statementName :: Lude.Maybe Lude.Text,
    withEvent :: Lude.Maybe Lude.Bool,
    database :: Lude.Text,
    sql :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftDataParameters' with the minimum fields required to make a request.
--
-- * 'database' - The name of the database. Required when authenticating using temporary credentials.
-- * 'dbUser' - The database user name. Required when authenticating using temporary credentials.
-- * 'secretManagerARN' - The name or ARN of the secret that enables access to the database. Required when authenticating using AWS Secrets Manager.
-- * 'sql' - The SQL statement text to run.
-- * 'statementName' - The name of the SQL statement. You can name the SQL statement when you create it to identify the query.
-- * 'withEvent' - Indicates whether to send an event back to EventBridge after the SQL statement runs.
mkRedshiftDataParameters ::
  -- | 'database'
  Lude.Text ->
  -- | 'sql'
  Lude.Text ->
  RedshiftDataParameters
mkRedshiftDataParameters pDatabase_ pSql_ =
  RedshiftDataParameters'
    { dbUser = Lude.Nothing,
      secretManagerARN = Lude.Nothing,
      statementName = Lude.Nothing,
      withEvent = Lude.Nothing,
      database = pDatabase_,
      sql = pSql_
    }

-- | The database user name. Required when authenticating using temporary credentials.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDBUser :: Lens.Lens' RedshiftDataParameters (Lude.Maybe Lude.Text)
rdpDBUser = Lens.lens (dbUser :: RedshiftDataParameters -> Lude.Maybe Lude.Text) (\s a -> s {dbUser = a} :: RedshiftDataParameters)
{-# DEPRECATED rdpDBUser "Use generic-lens or generic-optics with 'dbUser' instead." #-}

-- | The name or ARN of the secret that enables access to the database. Required when authenticating using AWS Secrets Manager.
--
-- /Note:/ Consider using 'secretManagerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpSecretManagerARN :: Lens.Lens' RedshiftDataParameters (Lude.Maybe Lude.Text)
rdpSecretManagerARN = Lens.lens (secretManagerARN :: RedshiftDataParameters -> Lude.Maybe Lude.Text) (\s a -> s {secretManagerARN = a} :: RedshiftDataParameters)
{-# DEPRECATED rdpSecretManagerARN "Use generic-lens or generic-optics with 'secretManagerARN' instead." #-}

-- | The name of the SQL statement. You can name the SQL statement when you create it to identify the query.
--
-- /Note:/ Consider using 'statementName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpStatementName :: Lens.Lens' RedshiftDataParameters (Lude.Maybe Lude.Text)
rdpStatementName = Lens.lens (statementName :: RedshiftDataParameters -> Lude.Maybe Lude.Text) (\s a -> s {statementName = a} :: RedshiftDataParameters)
{-# DEPRECATED rdpStatementName "Use generic-lens or generic-optics with 'statementName' instead." #-}

-- | Indicates whether to send an event back to EventBridge after the SQL statement runs.
--
-- /Note:/ Consider using 'withEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpWithEvent :: Lens.Lens' RedshiftDataParameters (Lude.Maybe Lude.Bool)
rdpWithEvent = Lens.lens (withEvent :: RedshiftDataParameters -> Lude.Maybe Lude.Bool) (\s a -> s {withEvent = a} :: RedshiftDataParameters)
{-# DEPRECATED rdpWithEvent "Use generic-lens or generic-optics with 'withEvent' instead." #-}

-- | The name of the database. Required when authenticating using temporary credentials.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpDatabase :: Lens.Lens' RedshiftDataParameters Lude.Text
rdpDatabase = Lens.lens (database :: RedshiftDataParameters -> Lude.Text) (\s a -> s {database = a} :: RedshiftDataParameters)
{-# DEPRECATED rdpDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | The SQL statement text to run.
--
-- /Note:/ Consider using 'sql' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdpSql :: Lens.Lens' RedshiftDataParameters Lude.Text
rdpSql = Lens.lens (sql :: RedshiftDataParameters -> Lude.Text) (\s a -> s {sql = a} :: RedshiftDataParameters)
{-# DEPRECATED rdpSql "Use generic-lens or generic-optics with 'sql' instead." #-}

instance Lude.FromJSON RedshiftDataParameters where
  parseJSON =
    Lude.withObject
      "RedshiftDataParameters"
      ( \x ->
          RedshiftDataParameters'
            Lude.<$> (x Lude..:? "DbUser")
            Lude.<*> (x Lude..:? "SecretManagerArn")
            Lude.<*> (x Lude..:? "StatementName")
            Lude.<*> (x Lude..:? "WithEvent")
            Lude.<*> (x Lude..: "Database")
            Lude.<*> (x Lude..: "Sql")
      )

instance Lude.ToJSON RedshiftDataParameters where
  toJSON RedshiftDataParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DbUser" Lude..=) Lude.<$> dbUser,
            ("SecretManagerArn" Lude..=) Lude.<$> secretManagerARN,
            ("StatementName" Lude..=) Lude.<$> statementName,
            ("WithEvent" Lude..=) Lude.<$> withEvent,
            Lude.Just ("Database" Lude..= database),
            Lude.Just ("Sql" Lude..= sql)
          ]
      )
