{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.GetClusterCredentials
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database user name and temporary password with temporary authorization to log on to an Amazon Redshift database. The action returns the database user name prefixed with @IAM:@ if @AutoCreate@ is @False@ or @IAMA:@ if @AutoCreate@ is @True@ . You can optionally specify one or more database user groups that the user will join at log on. By default, the temporary credentials expire in 900 seconds. You can optionally specify a duration between 900 seconds (15 minutes) and 3600 seconds (60 minutes). For more information, see <http://docs.aws.amazon.com/redshift/latest/mgmt/generating-user-credentials.html Using IAM Authentication to Generate Database User Credentials> in the Amazon Redshift Cluster Management Guide.
--
--
-- The AWS Identity and Access Management (IAM)user or role that executes GetClusterCredentials must have an IAM policy attached that allows access to all necessary actions and resources. For more information about permissions, see <http://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html#redshift-policy-resources.getclustercredentials-resources Resource Policies for GetClusterCredentials> in the Amazon Redshift Cluster Management Guide.
--
-- If the @DbGroups@ parameter is specified, the IAM policy must allow the @redshift:JoinGroup@ action with access to the listed @dbgroups@ .
--
-- In addition, if the @AutoCreate@ parameter is set to @True@ , then the policy must include the @redshift:CreateClusterUser@ privilege.
--
-- If the @DbName@ parameter is specified, the IAM policy must allow access to the resource @dbname@ for the specified database name.
--
module Network.AWS.Redshift.GetClusterCredentials
    (
    -- * Creating a Request
      getClusterCredentials
    , GetClusterCredentials
    -- * Request Lenses
    , gccDBGroups
    , gccDurationSeconds
    , gccAutoCreate
    , gccDBName
    , gccDBUser
    , gccClusterIdentifier

    -- * Destructuring the Response
    , getClusterCredentialsResponse
    , GetClusterCredentialsResponse
    -- * Response Lenses
    , gccrsDBUser
    , gccrsExpiration
    , gccrsDBPassword
    , gccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | The request parameters to get cluster credentials.
--
--
--
-- /See:/ 'getClusterCredentials' smart constructor.
data GetClusterCredentials = GetClusterCredentials'
  { _gccDBGroups          :: !(Maybe [Text])
  , _gccDurationSeconds   :: !(Maybe Int)
  , _gccAutoCreate        :: !(Maybe Bool)
  , _gccDBName            :: !(Maybe Text)
  , _gccDBUser            :: !Text
  , _gccClusterIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClusterCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccDBGroups' - A list of the names of existing database groups that the user named in @DbUser@ will join for the current session, in addition to any group memberships for an existing user. If not specified, a new user is added only to PUBLIC. Database group name constraints     * Must be 1 to 64 alphanumeric characters or hyphens     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.     * First character must be a letter.     * Must not contain a colon ( : ) or slash ( / ).      * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
-- * 'gccDurationSeconds' - The number of seconds until the returned temporary password expires. Constraint: minimum 900, maximum 3600. Default: 900
--
-- * 'gccAutoCreate' - Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
--
-- * 'gccDBName' - The name of a database that @DbUser@ is authorized to log on to. If @DbName@ is not specified, @DbUser@ can log on to any existing database. Constraints:     * Must be 1 to 64 alphanumeric characters or hyphens     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.     * First character must be a letter.     * Must not contain a colon ( : ) or slash ( / ).      * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
-- * 'gccDBUser' - The name of a database user. If a user name matching @DbUser@ exists in the database, the temporary user credentials have the same permissions as the existing user. If @DbUser@ doesn't exist in the database and @Autocreate@ is @True@ , a new user is created using the value for @DbUser@ with PUBLIC permissions. If a database user matching the value for @DbUser@ doesn't exist and @Autocreate@ is @False@ , then the command succeeds but the connection attempt will fail because the user doesn't exist in the database. For more information, see <http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER> in the Amazon Redshift Database Developer Guide.  Constraints:     * Must be 1 to 64 alphanumeric characters or hyphens. The user name can't be @PUBLIC@ .     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.     * First character must be a letter.     * Must not contain a colon ( : ) or slash ( / ).      * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
-- * 'gccClusterIdentifier' - The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
getClusterCredentials
    :: Text -- ^ 'gccDBUser'
    -> Text -- ^ 'gccClusterIdentifier'
    -> GetClusterCredentials
getClusterCredentials pDBUser_ pClusterIdentifier_ =
  GetClusterCredentials'
    { _gccDBGroups = Nothing
    , _gccDurationSeconds = Nothing
    , _gccAutoCreate = Nothing
    , _gccDBName = Nothing
    , _gccDBUser = pDBUser_
    , _gccClusterIdentifier = pClusterIdentifier_
    }


-- | A list of the names of existing database groups that the user named in @DbUser@ will join for the current session, in addition to any group memberships for an existing user. If not specified, a new user is added only to PUBLIC. Database group name constraints     * Must be 1 to 64 alphanumeric characters or hyphens     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.     * First character must be a letter.     * Must not contain a colon ( : ) or slash ( / ).      * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
gccDBGroups :: Lens' GetClusterCredentials [Text]
gccDBGroups = lens _gccDBGroups (\ s a -> s{_gccDBGroups = a}) . _Default . _Coerce

-- | The number of seconds until the returned temporary password expires. Constraint: minimum 900, maximum 3600. Default: 900
gccDurationSeconds :: Lens' GetClusterCredentials (Maybe Int)
gccDurationSeconds = lens _gccDurationSeconds (\ s a -> s{_gccDurationSeconds = a})

-- | Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
gccAutoCreate :: Lens' GetClusterCredentials (Maybe Bool)
gccAutoCreate = lens _gccAutoCreate (\ s a -> s{_gccAutoCreate = a})

-- | The name of a database that @DbUser@ is authorized to log on to. If @DbName@ is not specified, @DbUser@ can log on to any existing database. Constraints:     * Must be 1 to 64 alphanumeric characters or hyphens     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.     * First character must be a letter.     * Must not contain a colon ( : ) or slash ( / ).      * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
gccDBName :: Lens' GetClusterCredentials (Maybe Text)
gccDBName = lens _gccDBName (\ s a -> s{_gccDBName = a})

-- | The name of a database user. If a user name matching @DbUser@ exists in the database, the temporary user credentials have the same permissions as the existing user. If @DbUser@ doesn't exist in the database and @Autocreate@ is @True@ , a new user is created using the value for @DbUser@ with PUBLIC permissions. If a database user matching the value for @DbUser@ doesn't exist and @Autocreate@ is @False@ , then the command succeeds but the connection attempt will fail because the user doesn't exist in the database. For more information, see <http://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER> in the Amazon Redshift Database Developer Guide.  Constraints:     * Must be 1 to 64 alphanumeric characters or hyphens. The user name can't be @PUBLIC@ .     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.     * First character must be a letter.     * Must not contain a colon ( : ) or slash ( / ).      * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
gccDBUser :: Lens' GetClusterCredentials Text
gccDBUser = lens _gccDBUser (\ s a -> s{_gccDBUser = a})

-- | The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
gccClusterIdentifier :: Lens' GetClusterCredentials Text
gccClusterIdentifier = lens _gccClusterIdentifier (\ s a -> s{_gccClusterIdentifier = a})

instance AWSRequest GetClusterCredentials where
        type Rs GetClusterCredentials =
             GetClusterCredentialsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "GetClusterCredentialsResult"
              (\ s h x ->
                 GetClusterCredentialsResponse' <$>
                   (x .@? "DbUser") <*> (x .@? "Expiration") <*>
                     (x .@? "DbPassword")
                     <*> (pure (fromEnum s)))

instance Hashable GetClusterCredentials where

instance NFData GetClusterCredentials where

instance ToHeaders GetClusterCredentials where
        toHeaders = const mempty

instance ToPath GetClusterCredentials where
        toPath = const "/"

instance ToQuery GetClusterCredentials where
        toQuery GetClusterCredentials'{..}
          = mconcat
              ["Action" =: ("GetClusterCredentials" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "DbGroups" =:
                 toQuery (toQueryList "DbGroup" <$> _gccDBGroups),
               "DurationSeconds" =: _gccDurationSeconds,
               "AutoCreate" =: _gccAutoCreate,
               "DbName" =: _gccDBName, "DbUser" =: _gccDBUser,
               "ClusterIdentifier" =: _gccClusterIdentifier]

-- | Temporary credentials with authorization to log on to an Amazon Redshift database.
--
--
--
-- /See:/ 'getClusterCredentialsResponse' smart constructor.
data GetClusterCredentialsResponse = GetClusterCredentialsResponse'
  { _gccrsDBUser         :: !(Maybe Text)
  , _gccrsExpiration     :: !(Maybe ISO8601)
  , _gccrsDBPassword     :: !(Maybe (Sensitive Text))
  , _gccrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClusterCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccrsDBUser' - A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
--
-- * 'gccrsExpiration' - The date and time the password in @DbPassword@ expires.
--
-- * 'gccrsDBPassword' - A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ .
--
-- * 'gccrsResponseStatus' - -- | The response status code.
getClusterCredentialsResponse
    :: Int -- ^ 'gccrsResponseStatus'
    -> GetClusterCredentialsResponse
getClusterCredentialsResponse pResponseStatus_ =
  GetClusterCredentialsResponse'
    { _gccrsDBUser = Nothing
    , _gccrsExpiration = Nothing
    , _gccrsDBPassword = Nothing
    , _gccrsResponseStatus = pResponseStatus_
    }


-- | A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
gccrsDBUser :: Lens' GetClusterCredentialsResponse (Maybe Text)
gccrsDBUser = lens _gccrsDBUser (\ s a -> s{_gccrsDBUser = a})

-- | The date and time the password in @DbPassword@ expires.
gccrsExpiration :: Lens' GetClusterCredentialsResponse (Maybe UTCTime)
gccrsExpiration = lens _gccrsExpiration (\ s a -> s{_gccrsExpiration = a}) . mapping _Time

-- | A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ .
gccrsDBPassword :: Lens' GetClusterCredentialsResponse (Maybe Text)
gccrsDBPassword = lens _gccrsDBPassword (\ s a -> s{_gccrsDBPassword = a}) . mapping _Sensitive

-- | -- | The response status code.
gccrsResponseStatus :: Lens' GetClusterCredentialsResponse Int
gccrsResponseStatus = lens _gccrsResponseStatus (\ s a -> s{_gccrsResponseStatus = a})

instance NFData GetClusterCredentialsResponse where
