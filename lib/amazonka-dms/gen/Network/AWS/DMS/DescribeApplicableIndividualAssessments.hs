{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeApplicableIndividualAssessments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of individual assessments that you can specify for a new premigration assessment run, given one or more parameters.
--
-- If you specify an existing migration task, this operation provides the default individual assessments you can specify for that task. Otherwise, the specified parameters model elements of a possible migration task on which to base a premigration assessment run.
-- To use these migration task modeling parameters, you must specify an existing replication instance, a source database engine, a target database engine, and a migration type. This combination of parameters potentially limits the default individual assessments available for an assessment run created for a corresponding migration task.
-- If you specify no parameters, this operation provides a list of all possible individual assessments that you can specify for an assessment run. If you specify any one of the task modeling parameters, you must specify all of them or the operation cannot provide a list of individual assessments. The only parameter that you can specify alone is for an existing migration task. The specified task definition then determines the default list of individual assessments that you can specify in an assessment run for the task.
module Network.AWS.DMS.DescribeApplicableIndividualAssessments
    (
    -- * Creating a request
      DescribeApplicableIndividualAssessments (..)
    , mkDescribeApplicableIndividualAssessments
    -- ** Request lenses
    , daiaMarker
    , daiaMaxRecords
    , daiaMigrationType
    , daiaReplicationInstanceArn
    , daiaReplicationTaskArn
    , daiaSourceEngineName
    , daiaTargetEngineName

    -- * Destructuring the response
    , DescribeApplicableIndividualAssessmentsResponse (..)
    , mkDescribeApplicableIndividualAssessmentsResponse
    -- ** Response lenses
    , daiarrsIndividualAssessmentNames
    , daiarrsMarker
    , daiarrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeApplicableIndividualAssessments' smart constructor.
data DescribeApplicableIndividualAssessments = DescribeApplicableIndividualAssessments'
  { marker :: Core.Maybe Core.Text
    -- ^ Optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ Maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
  , migrationType :: Core.Maybe Types.MigrationTypeValue
    -- ^ Name of the migration type that each provided individual assessment must support.
  , replicationInstanceArn :: Core.Maybe Core.Text
    -- ^ ARN of a replication instance on which you want to base the default list of individual assessments.
  , replicationTaskArn :: Core.Maybe Core.Text
    -- ^ Amazon Resource Name (ARN) of a migration task on which you want to base the default list of individual assessments.
  , sourceEngineName :: Core.Maybe Core.Text
    -- ^ Name of a database engine that the specified replication instance supports as a source.
  , targetEngineName :: Core.Maybe Core.Text
    -- ^ Name of a database engine that the specified replication instance supports as a target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApplicableIndividualAssessments' value with any optional fields omitted.
mkDescribeApplicableIndividualAssessments
    :: DescribeApplicableIndividualAssessments
mkDescribeApplicableIndividualAssessments
  = DescribeApplicableIndividualAssessments'{marker = Core.Nothing,
                                             maxRecords = Core.Nothing,
                                             migrationType = Core.Nothing,
                                             replicationInstanceArn = Core.Nothing,
                                             replicationTaskArn = Core.Nothing,
                                             sourceEngineName = Core.Nothing,
                                             targetEngineName = Core.Nothing}

-- | Optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaMarker :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Core.Text)
daiaMarker = Lens.field @"marker"
{-# INLINEABLE daiaMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaMaxRecords :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Core.Int)
daiaMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE daiaMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | Name of the migration type that each provided individual assessment must support.
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaMigrationType :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Types.MigrationTypeValue)
daiaMigrationType = Lens.field @"migrationType"
{-# INLINEABLE daiaMigrationType #-}
{-# DEPRECATED migrationType "Use generic-lens or generic-optics with 'migrationType' instead"  #-}

-- | ARN of a replication instance on which you want to base the default list of individual assessments.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaReplicationInstanceArn :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Core.Text)
daiaReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE daiaReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | Amazon Resource Name (ARN) of a migration task on which you want to base the default list of individual assessments.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaReplicationTaskArn :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Core.Text)
daiaReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE daiaReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | Name of a database engine that the specified replication instance supports as a source.
--
-- /Note:/ Consider using 'sourceEngineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaSourceEngineName :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Core.Text)
daiaSourceEngineName = Lens.field @"sourceEngineName"
{-# INLINEABLE daiaSourceEngineName #-}
{-# DEPRECATED sourceEngineName "Use generic-lens or generic-optics with 'sourceEngineName' instead"  #-}

-- | Name of a database engine that the specified replication instance supports as a target.
--
-- /Note:/ Consider using 'targetEngineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaTargetEngineName :: Lens.Lens' DescribeApplicableIndividualAssessments (Core.Maybe Core.Text)
daiaTargetEngineName = Lens.field @"targetEngineName"
{-# INLINEABLE daiaTargetEngineName #-}
{-# DEPRECATED targetEngineName "Use generic-lens or generic-optics with 'targetEngineName' instead"  #-}

instance Core.ToQuery DescribeApplicableIndividualAssessments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeApplicableIndividualAssessments
         where
        toHeaders DescribeApplicableIndividualAssessments{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribeApplicableIndividualAssessments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeApplicableIndividualAssessments
         where
        toJSON DescribeApplicableIndividualAssessments{..}
          = Core.object
              (Core.catMaybes
                 [("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords,
                  ("MigrationType" Core..=) Core.<$> migrationType,
                  ("ReplicationInstanceArn" Core..=) Core.<$> replicationInstanceArn,
                  ("ReplicationTaskArn" Core..=) Core.<$> replicationTaskArn,
                  ("SourceEngineName" Core..=) Core.<$> sourceEngineName,
                  ("TargetEngineName" Core..=) Core.<$> targetEngineName])

instance Core.AWSRequest DescribeApplicableIndividualAssessments
         where
        type Rs DescribeApplicableIndividualAssessments =
             DescribeApplicableIndividualAssessmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeApplicableIndividualAssessmentsResponse' Core.<$>
                   (x Core..:? "IndividualAssessmentNames") Core.<*>
                     x Core..:? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDescribeApplicableIndividualAssessmentsResponse' smart constructor.
data DescribeApplicableIndividualAssessmentsResponse = DescribeApplicableIndividualAssessmentsResponse'
  { individualAssessmentNames :: Core.Maybe [Core.Text]
    -- ^ List of names for the individual assessments supported by the premigration assessment run that you start based on the specified request parameters. For more information on the available individual assessments, including compatibility with different migration task configurations, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs> in the /AWS Database Migration Service User Guide./ 
  , marker :: Core.Maybe Core.Text
    -- ^ Pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApplicableIndividualAssessmentsResponse' value with any optional fields omitted.
mkDescribeApplicableIndividualAssessmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeApplicableIndividualAssessmentsResponse
mkDescribeApplicableIndividualAssessmentsResponse responseStatus
  = DescribeApplicableIndividualAssessmentsResponse'{individualAssessmentNames
                                                       = Core.Nothing,
                                                     marker = Core.Nothing, responseStatus}

-- | List of names for the individual assessments supported by the premigration assessment run that you start based on the specified request parameters. For more information on the available individual assessments, including compatibility with different migration task configurations, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs> in the /AWS Database Migration Service User Guide./ 
--
-- /Note:/ Consider using 'individualAssessmentNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiarrsIndividualAssessmentNames :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse (Core.Maybe [Core.Text])
daiarrsIndividualAssessmentNames = Lens.field @"individualAssessmentNames"
{-# INLINEABLE daiarrsIndividualAssessmentNames #-}
{-# DEPRECATED individualAssessmentNames "Use generic-lens or generic-optics with 'individualAssessmentNames' instead"  #-}

-- | Pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiarrsMarker :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse (Core.Maybe Core.Text)
daiarrsMarker = Lens.field @"marker"
{-# INLINEABLE daiarrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiarrsResponseStatus :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse Core.Int
daiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
